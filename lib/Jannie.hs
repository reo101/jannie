{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Jannie (
  main,
) where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Applicative (asum)
import Control.Monad (guard)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace (traceM, traceShowM)
import qualified Discord as D
import qualified Discord.Interactions as DI
import qualified Discord.Requests as DR
import qualified Discord.Types as DT
import UnliftIO (liftIO)
import Utils (getGuildId, getToken)

-- MAIN

main :: IO ()
main = do
  _ <- loadFile defaultConfig
  tok <- getToken
  testserverid <- getGuildId

  -- open ghci and run  [[ :info RunDiscordOpts ]] to see available fields
  t <-
    D.runDiscord $
      D.def
        { D.discordToken = tok
        , D.discordOnStart = startHandler
        , D.discordOnEnd = liftIO $ putStrLn "Ended"
        , D.discordOnEvent = eventHandler testserverid
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

pattern Command ::
  T.Text ->
  Maybe T.Text ->
  Maybe DT.User ->
  Maybe DI.OptionsData ->
  DT.InteractionId ->
  DT.InteractionToken ->
  DT.GuildId ->
  DT.ChannelId ->
  DT.Event
pattern Command
  { name
  , nick
  , user
  , optionsData
  , interactionId
  , interactionToken
  , interactionGuildId
  , interactionChannelId
  } =
  DT.InteractionCreate
    DI.InteractionApplicationCommand
      { DI.applicationCommandData =
        DI.ApplicationCommandDataChatInput
          { DI.optionsData
          , DI.applicationCommandDataName = name
          }
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
eventHandler :: DT.GuildId -> DT.Event -> D.DiscordHandler ()
eventHandler testserverid event = case event of
  DT.Ready _ _ _ _ _ _ (DT.PartialApplication i _) -> do
    vs <-
      D.restCall $
        DR.BulkOverWriteGuildApplicationCommand
          i
          testserverid
          [ authenticateSlashCommand
          , rolesSlashCommand
          ]
    liftIO (putStrLn $ "number of application commands added " ++ show (length vs))
    acs <- D.restCall (DR.GetGuildApplicationCommands i testserverid)
    case acs of
      Left r -> liftIO $ print r
      Right ls -> liftIO $ putStrLn $ "number of application commands total " ++ show (length ls)
  -- Try to authenticate, but already have a nickname
  Command
    { name = "authenticate"
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
    { name = "authenticate"
    , nick = Nothing
    , user = Just (DT.User {DT.userId})
    , optionsData = Just (DI.OptionsDataValues optionsDataValues)
    , interactionId
    , interactionToken
    , interactionGuildId
    } -> do
      let getField field =
            fromJust $
              asum $
                fmap
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

      let name = getField "име"
      let fn = getField "фн"

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
                        Just "Успешно Ви бе генериран прякор. Добра работа, колега. Сега можете да си изберете кои групи искате да следите (в идното съобщение или с извикване на `/roles`)."
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
    { name = "roles"
    , nick = Just _
    , interactionChannelId
    } -> selectRoles interactionChannelId
  -- TODO: Apply roles
  DT.InteractionCreate
    DI.InteractionComponent
      { DI.componentData =
        DI.SelectMenuData
          { DI.componentDataCustomId = "role selection menu"
          , DI.componentDataValues = what
          }
      } -> do
      -- NOTE: (currently) cannot descructure componentDataValues into [Roles]
      traceM "Values"
      traceShowM what

      pure ()
  _ -> return ()

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

selectRoles :: DT.ChannelId -> D.DiscordHandler ()
selectRoles channelId = do
  void $
    D.restCall $
      DR.CreateMessageDetailed
        channelId
        ( DR.MessageDetailedOpts
            { DR.messageDetailedTTS = False
            , DR.messageDetailedStickerIds = Nothing
            , DR.messageDetailedReference = Nothing
            , DR.messageDetailedFile = Nothing
            , DR.messageDetailedEmbeds = Nothing
            , DR.messageDetailedContent = "Тук можеш да си избереш кои групи да следиш"
            , DR.messageDetailedComponents = Just [selectRolesComponent]
            , DR.messageDetailedAllowedMentions = Nothing
            }
        )
  traceShowM ()
  pure ()

fromBot :: DT.Message -> Bool
fromBot = DT.userIsBot . DT.messageAuthor

isPing :: DT.Message -> Bool
isPing = ("ping" `T.isPrefixOf`) . T.toLower . DT.messageContent

void :: (Show b) => D.DiscordHandler (Either D.RestCallErrorCode b) -> D.DiscordHandler ()
void =
  ( >>=
      ( \case
          Left e -> liftIO $ print e
          Right v -> liftIO $ print v
      )
  )
