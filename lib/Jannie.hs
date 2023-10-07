{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Jannie (
  main,
) where

import App (App, discordCall, liftDb, runDb)
import Command (Command' (..), readCommand)
import Config.Db qualified
import Config.Discord qualified
import Config.Discord qualified as Discord
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException, try)
import Control.Monad.Except
import Control.Monad.Extra (whileJustM)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (runReaderT)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty.Extra (maximum1, maximumOn1)
import Data.Maybe (mapMaybe)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Data.Text.IO qualified as Text.IO
import Data.Validation (Validation)
import Data.Validation qualified as Validation
import Database.Persist qualified as Persistent
import Database.Persist.Postgresql qualified as Persist.Postgresql
import Database.Persist.Sql.Migration qualified as Persistent
import Database.Persist.SqlBackend (SqlBackend)
import Discord qualified as D
import Discord.Interactions qualified as DI
import Discord.Requests qualified as DR
import Discord.Types qualified as DT
import Schema qualified
import Text.Printf (printf)
import User.FN qualified
import User.GithubUsername qualified
import User.Name qualified
import Utils (showText)

-- MAIN

main :: IO ()
main = do
  void $ try @SomeException $ loadFile defaultConfig
  readCommand >>= \case
    Command.Migrate {forReal, dbConfigFile} -> do
      dbConfig <- Config.Db.fromFileAndEnv dbConfigFile
      runStdoutLoggingT $
        Persist.Postgresql.withPostgresqlConn
          (Config.Db.toConnString dbConfig)
          \conn ->
            runReaderT
              ( if forReal
                  then Persistent.runMigration Schema.migrateAll
                  else Persistent.printMigration Schema.migrateAll
              )
              conn
    Command.Run {dbConfigFile, discordConfigFile} -> do
      config <- Config.Discord.fromFileAndEnv discordConfigFile
      dbConfig <- Config.Db.fromFileAndEnv dbConfigFile
      runStdoutLoggingT $
        Persist.Postgresql.withPostgresqlPool
          (Config.Db.toConnString dbConfig)
          100
          \connPool -> run connPool config

run :: (MonadIO m) => Pool SqlBackend -> Discord.Config -> m ()
run connPool discordConfig = do
  -- open ghci and run  [[ :info RunDiscordOpts ]] to see available fields
  t <-
    liftIO . D.runDiscord $
      D.def
        { D.discordToken = discordConfig.token.get
        , D.discordOnStart = startHandler
        , D.discordOnEnd = liftIO $ putStrLn "Ended"
        , D.discordOnEvent = runDb connPool . eventHandler discordConfig
        , D.discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
        , D.discordGatewayIntent =
            DT.GatewayIntent
              { DT.gatewayIntentWebhooks = True
              , DT.gatewayIntentVoiceStates = True
              , DT.gatewayIntentMessageTyping = True
              , DT.gatewayIntentMessageReactions = True
              , DT.gatewayIntentMessageContent = True
              , DT.gatewayIntentMessageChanges = True
              , DT.gatewayIntentInvites = True
              , DT.gatewayIntentIntegrations = True
              , DT.gatewayIntentGuilds = True
              , DT.gatewayIntentEmojis = True
              , DT.gatewayIntentDirectMessageTyping = True
              , DT.gatewayIntentDirectMessageReactions = True
              , DT.gatewayIntentDirectMessageChanges = True
              , DT.gatewayIntentBans = True
              , DT.gatewayIntentPresences = True
              , DT.gatewayIntentMembers = True
              }
        }
  liftIO $ TIO.putStrLn t

-- If the start handler throws an exception, discord-haskell will gracefully shutdown
--     Use place to execute commands you know you want to complete
startHandler :: D.DiscordHandler ()
startHandler = do
  let activity =
        D.def
          { DT.activityName = "over students 👀"
          , DT.activityType = DT.ActivityTypeWatching
          }
  let opts =
        DT.UpdateStatusOpts
          { DT.updateStatusOptsSince = Nothing
          , DT.updateStatusOptsGame = Just activity
          , DT.updateStatusOptsNewStatus = DT.UpdateStatusOnline
          , DT.updateStatusOptsAFK = False
          }
  D.sendCommand (DT.UpdateStatus opts)

-- | Authentication command
authenticateSlashCommand :: DI.CreateApplicationCommand
authenticateSlashCommand =
  DI.CreateApplicationCommandChatInput
    { DI.createName = "authenticate"
    , DI.createLocalizedName = Nothing
    , DI.createDescription = "Authentication Helper"
    , DI.createLocalizedDescription = Nothing
    , DI.createDefaultMemberPermissions = Nothing
    , DI.createDMPermission = Nothing
    , DI.createOptions =
        Just $
          DI.OptionsValues
            [ DI.OptionValueString
                { DI.optionValueName = "име"
                , DI.optionValueLocalizedName = Nothing
                , DI.optionValueDescription = "Име и Фамилия (на кирилица)"
                , DI.optionValueLocalizedDescription = Nothing
                , DI.optionValueRequired = True
                , DI.optionValueStringChoices = Left False
                , DI.optionValueStringMinLen = Just 1
                , DI.optionValueStringMaxLen = Just 50
                }
            , DI.OptionValueString
                { DI.optionValueName = "фн"
                , DI.optionValueLocalizedName = Nothing
                , DI.optionValueDescription = "Факултетен Номер"
                , DI.optionValueLocalizedDescription = Nothing
                , DI.optionValueRequired = True
                , DI.optionValueStringChoices = Left False
                , DI.optionValueStringMinLen = Just 5
                , DI.optionValueStringMaxLen = Just 10
                }
            ]
    }

-- | Authentication command
githubSlashCommand :: DI.CreateApplicationCommand
githubSlashCommand =
  DI.CreateApplicationCommandChatInput
    { DI.createName = "github"
    , DI.createLocalizedName = Nothing
    , DI.createDescription = "GitHub Username Helper"
    , DI.createLocalizedDescription = Nothing
    , DI.createDefaultMemberPermissions = Just "0"
    , DI.createDMPermission = Nothing
    , DI.createOptions =
        Just $
          DI.OptionsValues
            [ DI.OptionValueString
                { DI.optionValueName = "github"
                , DI.optionValueLocalizedName = Nothing
                , DI.optionValueDescription = "Username в GitHub (https://github.com/{USERNAME})"
                , DI.optionValueLocalizedDescription = Nothing
                , DI.optionValueRequired = True
                , DI.optionValueStringChoices = Left False
                , DI.optionValueStringMinLen = Just 1
                , DI.optionValueStringMaxLen = Just 40
                }
            ]
    }

syncSlashCommand :: DI.CreateApplicationCommand
syncSlashCommand =
  DI.CreateApplicationCommandChatInput
    { DI.createName = "sync"
    , DI.createLocalizedName = Nothing
    , DI.createDescription = "TODO: doesn't work. Sync users to db"
    , DI.createLocalizedDescription = Nothing
    , DI.createDefaultMemberPermissions = Just "0"
    , DI.createDMPermission = Nothing
    , DI.createOptions = Nothing
    }

pattern DataChatInput ::
  T.Text ->
  Maybe DI.OptionsData ->
  DI.ApplicationCommandData
pattern DataChatInput
  { commandName
  , optionsData
  } <-
  DI.ApplicationCommandDataChatInput
    { DI.optionsData
    , DI.applicationCommandDataName = commandName
    }

pattern Command ::
  Maybe T.Text ->
  Maybe DT.User ->
  [DT.RoleId] ->
  DI.ApplicationCommandData ->
  DT.InteractionId ->
  DT.InteractionToken ->
  DT.GuildId ->
  DT.Event
pattern Command
  { nick
  , user
  , roles
  , commandData
  , interactionId
  , interactionToken
  , interactionGuildId
  } <-
  DT.InteractionCreate
    DI.InteractionApplicationCommand
      { DI.applicationCommandData = commandData
      , DI.interactionId
      , DI.interactionToken
      , DI.interactionGuildId = Just interactionGuildId
      , DI.interactionUser =
        DI.MemberOrUser
          ( Left
              ( DT.GuildMember
                  { DT.memberUser = user
                  , DT.memberNick = nick
                  , DT.memberRoles = roles
                  }
                )
            )
      }

-- If an event handler throws an exception, discord-haskell will continue to run
eventHandler :: Discord.Config -> DT.Event -> App ()
eventHandler config@(Discord.Config {guildId, defaultRoles}) event = case event of
  DT.Ready _ _ _ _ _ _ (DT.PartialApplication i _) -> do
    vs <-
      discordCall $
        DR.BulkOverWriteGuildApplicationCommand
          i
          guildId
          [ authenticateSlashCommand
          , githubSlashCommand
          , syncSlashCommand
          ]
    liftIO (putStrLn $ "number of application commands added " ++ show (length vs))
    acs <- discordCall (DR.GetGuildApplicationCommands i guildId)
    case acs of
      Left r -> liftIO $ print r
      Right ls -> liftIO $ putStrLn $ "number of application commands total " ++ show (length ls)
  -- Try to authenticate, but already have a nickname
  Command
    { commandData =
      DataChatInput
        { commandName = "authenticate"
        }
    , nick = Just nick
    , interactionId
    , interactionToken
    } -> do
      printError_ $
        discordCall $
          DR.CreateInteractionResponse
            interactionId
            interactionToken
            ( DI.InteractionResponseChannelMessage
                ( DI.InteractionResponseMessage
                    { DI.interactionResponseMessageTTS = Nothing
                    , DI.interactionResponseMessageContent =
                        Just $ "Колега, вече Ви бе генериран прякора " <> nick <> ". Ако искате да го подмените, пишете на администратор, че да Ви махне стария."
                    , DI.interactionResponseMessageAttachments = Nothing
                    , DI.interactionResponseMessageAllowedMentions = Nothing
                    , DI.interactionResponseMessageComponents = Nothing
                    , DI.interactionResponseMessageEmbeds = Nothing
                    , DI.interactionResponseMessageFlags =
                        Just $
                          DI.InteractionResponseMessageFlags
                            [ DI.InteractionResponseMessageFlagEphermeral
                            ]
                    }
                )
            )
  -- Authenticate, setting a nickname
  Command
    { commandData =
      DataChatInput
        { commandName = "authenticate"
        , optionsData = Just (DI.OptionsDataValues optionsDataValues)
        }
    , nick = Nothing
    , user = Just (DT.User {DT.userId})
    , interactionId
    , interactionToken
    , interactionGuildId
    } -> do
      let getField field =
            head $
              mapMaybe
                ( \case
                    DI.OptionDataValueString
                      { DI.optionDataValueName = fieldName
                      , DI.optionDataValueString = Right fieldValue
                      } -> do
                        guard $ field == fieldName
                        pure fieldValue
                    _ -> Nothing
                )
                optionsDataValues

      let reply = replyEphemeral interactionId interactionToken

      let name = getField "име"
      let fn = getField "фн"

      let user :: Validation (NonEmpty Text) Schema.User
          user = do
            name <-
              Validation.validationNel $
                tryParse
                  User.Name.parse
                  name
                  "Не Ви е валидно името, колега!"
                  User.Name.regexPattern
            fn <-
              Validation.validationNel $
                tryParse
                  User.FN.parse
                  fn
                  "Не Ви е валиден факултетният номер, колега!"
                  User.FN.regexPattern
            pure Schema.User {..}

      case user of
        Validation.Failure errors -> reply $ Text.unlines $ NonEmpty.toList errors
        Validation.Success user -> do
          -- Set nickname
          liftDb $ Persistent.insert user
          printError_ $
            discordCall $
              DR.ModifyGuildMember
                interactionGuildId
                userId
                ( D.def
                    { DR.modifyGuildMemberOptsNickname =
                        Just $ name <> " - " <> fn
                    }
                )

          -- Set default roles for user after authentication
          printError_ $
            discordCall $
              DR.ModifyGuildMember
                interactionGuildId
                userId
                ( D.def
                    { DR.modifyGuildMemberOptsRoles = Just defaultRoles
                    }
                )

          -- (Privately) Report success and prompt the manual selection of channels to follow
          replyEphemeral interactionId interactionToken $
            Text.unlines
              [ "Успешно Ви бе генериран прякор. Добра работа, колега <@!" <> showText userId <> ">!"
              , let roleMentions :: [Text]
                    roleMentions = map (Text.pack . printf "<@&%s>" . show) defaultRoles
                 in "Автоматично получавате ролите: " <> Text.intercalate ", " roleMentions <> "."
              , "Сега можете да навигирате до <id:customize> и да си изберете кои групи да следите."
              ]
  -- Set a github username
  Command
    { commandData =
      DataChatInput
        { commandName = "github"
        , optionsData = Just (DI.OptionsDataValues optionsDataValues)
        }
    , nick = Just _nick -- only previously authenticated users will see use this command (for now manually set up for role Студент with overridden permissions in the server)
    , user = Just (DT.User {DT.userId})
    , interactionId
    , interactionToken
    } -> do
      let getField field =
            head $
              mapMaybe
                ( \case
                    DI.OptionDataValueString
                      { DI.optionDataValueName = fieldName
                      , DI.optionDataValueString = Right fieldValue
                      } -> do
                        guard $ field == fieldName
                        pure fieldValue
                    _ -> Nothing
                )
                optionsDataValues

      let reply = replyEphemeral interactionId interactionToken

      let github = getField "github"

      let error =
            tryParse
              User.GithubUsername.parse
              github
              "Не Ви е валиден GitHub username-ът, колега!"
              User.GithubUsername.regexPattern

      case error of
        Left error -> reply error
        -- TODO: add github to db, possibly parse from user name?
        Right _github ->
          replyEphemeral interactionId interactionToken $
            Text.unlines
              [ "Успешно ни пошепнахте своето име в GitHub: " <> github <> ". Добра работа, колега <@!" <> showText userId <> ">!"
              ]
  Command
    { commandData =
      DataChatInput
        { commandName = "sync"
        }
    , nick = Just _
    , user = Just _
    , roles
    , interactionId
    , interactionToken
    , interactionGuildId
    } | config.rootRole `elem` roles -> do
      let
        reply = replyEphemeral interactionId interactionToken
        getFN :: Text -> Validation (NonEmpty Text) Schema.User
        getFN nick =
          let parseErr = "could not parse " <> nick <> "'s FN"
           in case Text.splitOn "-" nick of
                [unparsedName, unparsedFn]
                  | Just name <- User.Name.parse (Text.dropEnd 1 unparsedName)
                  , Just fn <- User.FN.parse (Text.drop 1 unparsedFn) ->
                      Validation.Success $ Schema.User name fn
                _ -> Validation.Failure $ NonEmpty.singleton parseErr
      usersMay <-
        getAllUsers interactionGuildId
      case usersMay of
        Left err -> reply $ showText err
        Right users -> do
          reply $ "Pre-filter:" <> showText users
          let
            -- skip root users, they don't need to be logged
            notRoot user = config.rootRole `notElem` user.memberRoles
            -- skip users which have not assigned themselves yet
            assignedRole user = user.memberRoles /= config.defaultRoles
            skippedUsers = filter (\user -> notRoot user && assignedRole user) users
          case traverse getFN $ mapMaybe (.memberNick) skippedUsers of
            Validation.Failure err -> reply $ Text.unlines $ NonEmpty.toList err
            Validation.Success users -> do
              -- TODO: maybe err if missing?
              liftDb $ Persistent.putMany users
  _ -> return ()
  where
    replyEphemeral :: DT.InteractionId -> DT.InteractionToken -> Text -> App ()
    replyEphemeral interactionId interactionToken message =
      printError_ $
        discordCall $
          DR.CreateInteractionResponse
            interactionId
            interactionToken
            ( DI.InteractionResponseChannelMessage
                ( DI.InteractionResponseMessage
                    { DI.interactionResponseMessageTTS = Nothing
                    , DI.interactionResponseMessageContent =
                        Just message
                    , DI.interactionResponseMessageAttachments = Nothing
                    , DI.interactionResponseMessageAllowedMentions = Nothing
                    , DI.interactionResponseMessageComponents = Nothing
                    , DI.interactionResponseMessageEmbeds = Nothing
                    , DI.interactionResponseMessageFlags =
                        Just $
                          DI.InteractionResponseMessageFlags
                            [ DI.InteractionResponseMessageFlagEphermeral
                            ]
                    }
                )
            )

tryParse :: (a -> Maybe b) -> a -> Text -> String -> Either Text b
tryParse parse input errorMessage pttern =
  case parse input of
    Nothing -> Left $ errorMessage <> " Гоним нещо като " <> Text.pack pttern
    Just b -> Right b

printError_ :: (MonadIO m) => m (Either D.RestCallErrorCode b) -> m ()
printError_ =
  ( >>=
      ( \case
          Left e -> liftIO $ print e
          -- Right v -> liftIO $ print v
          Right _ -> pure ()
      )
  )

{- | Fetch all users in the guild.
Note that simply calling with no 'guildMembersTimingLimit' doesn't work since discord then defaults it to 1.
-}
getAllUsers :: DT.GuildId -> App (Either D.RestCallErrorCode [DT.GuildMember])
getAllUsers interactionGuildId = runExceptT $ getBatch 0
  where
    getBatch :: DT.UserId -> ExceptT D.RestCallErrorCode App [DT.GuildMember]
    getBatch lastId = do
      res <-
        ExceptT $
          discordCall $
            DR.ListGuildMembers
              interactionGuildId
              ( DR.GuildMembersTiming
                  { -- Fetch a 1000 users after lastId, ordered by id.
                    -- Note that 1000 is the limit discord imposes
                    guildMembersTimingLimit = Just 1000
                  , guildMembersTimingAfter = Just lastId
                  }
              )
      if length res < 1000
        then pure res
        else case nonEmpty $ mapMaybe (fmap (.userId) . (.memberUser)) res of
          Nothing -> error "sholdn't happen, checked for length above, and we're fetching users"
          Just neRes -> (res ++) <$> getBatch (maximum1 neRes)
