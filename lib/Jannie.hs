{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Jannie (
  main,
) where

import Config (Config (..), getConfig)
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad (guard, unless)
import Control.Monad.State.Lazy (execState, modify)
import Data.Foldable (traverse_)
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Discord qualified as D
import Discord.Interactions qualified as DI
import Discord.Requests qualified as DR
import Discord.Types qualified as DT
import Text.Printf (printf)
import Text.Regex.TDFA ((=~))
import UnliftIO (liftIO)

-- MAIN

main :: IO ()
main = do
  _ <- loadFile defaultConfig

  config@Config {token} <- getConfig

  -- open ghci and run  [[ :info RunDiscordOpts ]] to see available fields
  t <-
    D.runDiscord $
      D.def
        { D.discordToken = token
        , D.discordOnStart = startHandler
        , D.discordOnEnd = liftIO $ putStrLn "Ended"
        , D.discordOnEvent = eventHandler config
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
  TIO.putStrLn t

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
  DI.ApplicationCommandData ->
  DT.InteractionId ->
  DT.InteractionToken ->
  DT.GuildId ->
  DT.ChannelId ->
  DT.Event
pattern Command
  { nick
  , user
  , commandData
  , interactionId
  , interactionToken
  , interactionGuildId
  , interactionChannelId
  } <-
  DT.InteractionCreate
    DI.InteractionApplicationCommand
      { DI.applicationCommandData = commandData
      , DI.interactionId
      , DI.interactionToken
      , DI.interactionGuildId = Just interactionGuildId
      , DI.interactionChannelId = Just interactionChannelId
      , DI.interactionUser =
        DI.MemberOrUser
          ( Left
              ( DT.GuildMember
                  { DT.memberUser = user
                  , DT.memberNick = nick
                  }
                )
            )
      }

-- If an event handler throws an exception, discord-haskell will continue to run
eventHandler :: Config -> DT.Event -> D.DiscordHandler ()
eventHandler (Config {guildId, defaultRoles}) event = case event of
  DT.Ready _ _ _ _ _ _ (DT.PartialApplication i _) -> do
    vs <-
      D.restCall $
        DR.BulkOverWriteGuildApplicationCommand
          i
          guildId
          [ authenticateSlashCommand
          , githubSlashCommand
          ]
    liftIO (putStrLn $ "number of application commands added " ++ show (length vs))
    acs <- D.restCall (DR.GetGuildApplicationCommands i guildId)
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
      void $
        D.restCall $
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

      let validate :: [(T.Text, String, String)] -> Maybe (D.DiscordHandler ())
          validate instructions = do
            let errors =
                  flip execState [] $
                    traverse_
                      ( \(fieldValue, fieldPattern, message) ->
                          unless (fieldValue =~ fieldPattern) $
                            modify $
                              (:) $
                                message <> " Гоним нещо като " <> fieldPattern
                      )
                      instructions

            if null errors
              then Nothing
              else Just $ reply (unlines errors)

      let name = getField "име"
      let fn = getField "фн"

      let errors =
            validate
              [ (,,)
                  name
                  "^[А-Я][а-я]+ [А-Я][а-я]+$"
                  "Не Ви е валидно името, колега!"
              , (,,)
                  fn
                  "^([0-9]{5}|[0-9]MI[0-9]{7})$"
                  "Не Ви е валиден факултетният номер, колега!"
              ]

      case errors of
        Just callback -> callback
        Nothing -> do
          -- Set nickname
          void $
            D.restCall $
              DR.ModifyGuildMember
                interactionGuildId
                userId
                ( D.def
                    { DR.modifyGuildMemberOptsNickname =
                        Just $ name <> " - " <> fn
                    }
                )

          -- Set default roles for user after authentication
          void $
            D.restCall $
              DR.ModifyGuildMember
                interactionGuildId
                userId
                ( D.def
                    { DR.modifyGuildMemberOptsRoles = Just defaultRoles
                    }
                )

          -- (Privately) Report success and prompt the manual selection of channels to follow
          replyEphemeral interactionId interactionToken $
            unlines
              [ "Успешно Ви бе генериран прякор. Добра работа, колега <@!" <> show userId <> ">!"
              , let roleMentions :: [String]
                    roleMentions = map (printf "<@&%s>" . show) defaultRoles
                 in "Автоматично получавате ролите: " <> intercalate ", " roleMentions <> "."
              , "Сега можете да навигирате до <id:customize> и да си изберете кои групи да следите."
              ]
  -- Set a github username
  Command
    { commandData =
      DataChatInput
        { commandName = "github"
        , optionsData = Just (DI.OptionsDataValues optionsDataValues)
        }
    , nick = Just nick -- only previously authenticated users will see use this command (for now manually set up for role Студент with overridden permissions in the server)
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

      let validate :: [(T.Text, String, String)] -> Maybe (D.DiscordHandler ())
          validate instructions = do
            let errors =
                  flip execState [] $
                    traverse_
                      ( \(fieldValue, fieldPattern, message) ->
                          unless (fieldValue =~ fieldPattern) $
                            modify $
                              (:) $
                                message <> " Гоним нещо като " <> fieldPattern
                      )
                      instructions

            if null errors
              then Nothing
              else Just $ reply (unlines errors)

      let github = getField "github"

      let errors =
            validate
              [ (,,) github "^[a-zA-Z0-9]([-a-zA-Z0-9]{0,38}[a-zA-Z0-9])?$" "Не Ви е валиден GitHub username-а, колега!"
              ]

      case errors of
        Just callback -> callback
        Nothing ->
          replyEphemeral interactionId interactionToken $
            unlines
              [ "Успешно ни пошепнахте своето име в GitHub: " <> T.unpack github <> ". Добра работа, колега <@!" <> show userId <> ">!"
              ]
  _ -> return ()
  where
    replyEphemeral :: DT.InteractionId -> DT.InteractionToken -> String -> D.DiscordHandler ()
    replyEphemeral interactionId interactionToken message =
      void $
        D.restCall $
          DR.CreateInteractionResponse
            interactionId
            interactionToken
            ( DI.InteractionResponseChannelMessage
                ( DI.InteractionResponseMessage
                    { DI.interactionResponseMessageTTS = Nothing
                    , DI.interactionResponseMessageContent =
                        Just $ T.pack message
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

void :: (Show b) => D.DiscordHandler (Either D.RestCallErrorCode b) -> D.DiscordHandler ()
void =
  ( >>=
      ( \case
          Left e -> liftIO $ print e
          -- Right v -> liftIO $ print v
          Right _ -> pure ()
      )
  )
