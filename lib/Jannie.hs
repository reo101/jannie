{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Jannie (
  main,
) where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad (guard, unless)
import Data.List (stripPrefix)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace (traceShowM)
import qualified Discord as D
import qualified Discord.Interactions as DI
import qualified Discord.Internal.Types.Prelude as DITP
import qualified Discord.Requests as DR
import qualified Discord.Types as DT
import Text.Read (readMaybe)
import UnliftIO (liftIO)
import Utils (getGuildId, getToken, getAdminRoles)
import Config ( Config(..) )
import Text.Regex.TDFA ((=~))
import Data.Foldable (traverse_)
import Control.Monad.State.Lazy (execState, modify)

-- MAIN

main :: IO ()
main = do
  _ <- loadFile defaultConfig
  tok <- getToken
  guildId <- getGuildId
  adminRoles <- getAdminRoles

  let config = Config.Config { guildId, adminRoles }

  -- open ghci and run  [[ :info RunDiscordOpts ]] to see available fields
  t <-
    D.runDiscord $
      D.def
        { D.discordToken = tok
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

rolesSlashCommand :: DI.CreateApplicationCommand
rolesSlashCommand =
  DI.CreateApplicationCommandChatInput
    { DI.createName = "roles"
    , DI.createLocalizedName = Nothing
    , DI.createDescription = "Roles Selector"
    , DI.createLocalizedDescription = Nothing
    , DI.createDefaultMemberPermissions = Nothing
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
eventHandler (Config { guildId, adminRoles }) event = case event of
  DT.Ready _ _ _ _ _ _ (DT.PartialApplication i _) -> do
    vs <-
      D.restCall $
        DR.BulkOverWriteGuildApplicationCommand
          i
          guildId
          [ authenticateSlashCommand
          , rolesSlashCommand
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
                      (\(fieldValue, fieldPattern, message) ->
                        unless (fieldValue =~ fieldPattern) $
                          modify $ (:) $ message <> " Гоним нещо като " <> fieldPattern)
                      instructions

            if null errors
              then Nothing
              else Just $ reply (unlines errors)


      let name = getField "име"
      let fn = getField "фн"
      
      let errors = 
            validate
              [ (,,) name "^[А-Я][а-я]+ [А-Я][а-я]+$"     "Не Ви е валидно името, колега!"
              , (,,) fn   "^([0-9]{5}|[0-9]MI[0-9]{7})$"  "Не Ви е валиден факултетният номер, колега!"
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

          -- (Privately) Report success and prompt the selection of roles
          void $
            D.restCall $
              DR.CreateInteractionResponse
                interactionId
                interactionToken
                ( DI.InteractionResponseChannelMessage
                    ( DI.InteractionResponseMessage
                        { DI.interactionResponseMessageTTS = Nothing
                        , DI.interactionResponseMessageContent =
                            Just "Успешно Ви бе генериран прякор. Добра работа, колега. Сега можете да си изберете кои групи искате да следите (от полето долу или с извикване на `/roles`)."
                        , DI.interactionResponseMessageAttachments = Nothing
                        , DI.interactionResponseMessageAllowedMentions = Nothing
                        , DI.interactionResponseMessageComponents =
                            Just
                              [ selectRolesComponent
                              ]
                        , DI.interactionResponseMessageEmbeds = Nothing
                        , DI.interactionResponseMessageFlags =
                            Just $
                              DI.InteractionResponseMessageFlags
                                [ DI.InteractionResponseMessageFlagEphermeral
                                ]
                        }
                    )
                )
      
  -- Select roles
  Command
    { commandData =
      DataChatInput
        { commandName = "roles"
        }
    , nick = Just _
    , interactionId
    , interactionToken
    } ->
      void $
        D.restCall $
          DR.CreateInteractionResponse
            interactionId
            interactionToken
            ( DI.InteractionResponseChannelMessage
                ( DI.InteractionResponseMessage
                    { DI.interactionResponseMessageTTS = Nothing
                    , -- TODO: notify user that admin roles are not set
                      DI.interactionResponseMessageContent =
                        Just "Тук можете да си изберете кои групи да следите"
                    , DI.interactionResponseMessageAttachments = Nothing
                    , DI.interactionResponseMessageAllowedMentions = Nothing
                    , DI.interactionResponseMessageComponents =
                        Just
                          [ selectRolesComponent
                          ]
                    , DI.interactionResponseMessageEmbeds = Nothing
                    , DI.interactionResponseMessageFlags =
                        Just $
                          DI.InteractionResponseMessageFlags
                            [ DI.InteractionResponseMessageFlagEphermeral
                            ]
                    }
                )
            )
  -- Apply roles
  -- TODO: make pattern
  DT.InteractionCreate
    DI.InteractionComponent
      { DI.componentData =
        DI.SelectMenuData
          { DI.componentDataCustomId = "role selection menu"
          , DI.componentDataValues = internalRoles
          }
      , DI.interactionUser =
        DI.MemberOrUser
          ( Left
              ( DT.GuildMember
                  { DT.memberUser = Just (DT.User {DT.userId})
                  }
                )
            )
      , DI.interactionGuildId = Just interactionGuildId
      , DI.interactionId
      , DI.interactionToken
      } -> do
      -- HACK: Since the real `SelectMenuDataRole` type is not exported, the only thing we can do is to use its `Show` instance to scoop out the underlying list of `RoleID`s (`Snowflake`s)
      let roles :: Maybe [DITP.RoleId]
          roles = do
            let stringRep = show internalRoles
            snowFlakeArray <- stripPrefix "SelectMenuDataRole " stringRep
            unfilteredRoles <- readMaybe snowFlakeArray
            -- TODO: if they try to set any adminRoles, log them in channel #hall-of-haxxors :D
            pure $ filter (`notElem` adminRoles) unfilteredRoles

      -- traceShowM roles

      -- Set roles
      void $
        D.restCall $
          DR.ModifyGuildMember
            interactionGuildId
            userId
            ( D.def
                { DR.modifyGuildMemberOptsRoles = roles
                }
            )

      -- Notify user about successful setting of roles
      void $
        D.restCall $
          DR.CreateInteractionResponse
            interactionId
            interactionToken
            ( DI.InteractionResponseChannelMessage
                ( DI.InteractionResponseMessage
                    { DI.interactionResponseMessageTTS = Nothing
                    , -- TODO: notify user that admin roles are not set
                      DI.interactionResponseMessageContent =
                        Just "Чудесно! Успешно променихте ролите си!"
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


selectRolesComponent :: DT.ActionRow
selectRolesComponent =
  DT.ActionRowSelectMenu
    ( DT.SelectMenu
        { DT.selectMenuCustomId = "role selection menu"
        , DT.selectMenuDisabled = False
        , DT.selectMenuData = DT.SelectMenuDataRole
        , DT.selectMenuPlaceholder = Nothing
        , DT.selectMenuMinValues = Just 0
        , DT.selectMenuMaxValues = Just 10
        }
    )

fromBot :: DT.Message -> Bool
fromBot = DT.userIsBot . DT.messageAuthor

isPing :: DT.Message -> Bool
isPing = ("ping" `T.isPrefixOf`) . T.toLower . DT.messageContent

void :: (Show b) => D.DiscordHandler (Either D.RestCallErrorCode b) -> D.DiscordHandler ()
void =
  ( >>=
      ( \case
          Left e -> liftIO $ print e
          -- Right v -> liftIO $ print v
          Right _ -> pure ()
      )
  )
