{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Jannie (
  main,
) where

import App (discordCall)
import Command (Command' (..), readCommand)
import Config.Discord qualified
import Config.Discord qualified as Discord
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException, try)
import Control.Monad.Except
import Control.Monad.Extra (guard, void, whileJustM)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (MonadIO, liftIO, runReaderT)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty.Extra (maximum1, maximumOn1)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Data.Text.IO qualified as Text.IO
import Data.Validation (Validation)
import Data.Validation qualified as Validation
import Discord qualified as D
import Discord.Interactions qualified as DI
import Discord.Requests qualified as DR
import Discord.Types qualified as DT
import Text.Printf (printf)
import User.FN qualified
import User.Name qualified
import Utils (showText)
import Discord (DiscordHandler)

-- MAIN

main :: IO ()
main = do
  void $ try @SomeException $ loadFile defaultConfig
  readCommand >>= \case
    Command.Run {discordConfigFile} -> do
      config <- Config.Discord.fromFileAndEnv discordConfigFile
      run config

run :: (MonadIO m) => Discord.Config -> m ()
run discordConfig = do
  -- open ghci and run  [[ :info RunDiscordOpts ]] to see available fields
  t <-
    liftIO . D.runDiscord $
      D.def
        { D.discordToken = discordConfig.token.get
        , D.discordOnStart = startHandler
        , D.discordOnEnd = liftIO $ putStrLn "Ended"
        , D.discordOnEvent = eventHandler discordConfig
        , D.discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
        , D.discordGatewayIntent =
            DT.GatewayIntent
              { DT.gatewayIntentAutoModerationExecution = True
              , DT.gatewayIntentAutoModerationConfiguration = True
              , DT.gatewayIntentWebhooks = True
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
        DT.Activity
          { DT.activityName = "over students 👀"
          , DT.activityType = DT.ActivityTypeWatching
          , DT.activityUrl = Nothing
          , DT.activityCreatedAt = 0
          , DT.activityTimeStamps = Nothing
          , DT.activityApplicationId = Nothing
          , DT.activityDetails = Nothing
          , DT.activityState = Nothing
          , DT.activityEmoji = Nothing
          , DT.activityParty = Nothing
          , DT.activityInstance = Nothing
          , DT.activityFlags = Nothing
          , DT.activityButtons = Nothing
          }
  let opts =
        DT.UpdateStatusOpts
          { DT.updateStatusOptsSince = Nothing
          , DT.updateStatusOptsActivities = pure activity
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
eventHandler :: Discord.Config -> DT.Event -> DiscordHandler ()
eventHandler config@(Discord.Config {guildId, defaultRoles}) event = case event of
  DT.Ready _ _ _ _ _ _ (DT.PartialApplication i _) -> do
    vs <-
      discordCall $
        DR.BulkOverWriteGuildApplicationCommand
          i
          guildId
          [ authenticateSlashCommand
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

      let user :: Validation (NonEmpty Text) (User.Name.Name, User.FN.FN)
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
            pure (name, fn)

      case user of
        Validation.Failure errors -> reply $ Text.unlines $ NonEmpty.toList errors
        Validation.Success user -> do
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
  _ -> return ()
  where
    replyEphemeral :: DT.InteractionId -> DT.InteractionToken -> Text -> DiscordHandler ()
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
