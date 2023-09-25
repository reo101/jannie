module Utils where

import qualified Data.Text as T
import qualified Discord as D
import qualified Discord.Requests as R
import qualified Discord.Types as DT
import Text.Read (readMaybe)
import System.Environment (lookupEnv)

getAdminRoles :: IO [DT.RoleId]
getAdminRoles = do
  maybeRawAdminRoles <- lookupEnv "ADMIN_ROLES"
  case maybeRawAdminRoles of
    Nothing -> error "$ADMIN_ROLES not set"
    Just rawAdminRoles -> case readMaybe rawAdminRoles of
      Nothing -> error "$ADMIN_ROLES wrong format"
      Just adminRoles -> pure adminRoles

getToken :: IO T.Text
getToken = do
  maybeToken <- lookupEnv "AUTH_TOKEN"
  case maybeToken of
    Just token -> pure $ T.pack token
    Nothing -> error "$AUTH_TOKEN not set"

getGuildId :: IO DT.GuildId
getGuildId = do
  maybeGid <- lookupEnv "GUILD_ID"
  case maybeGid of
    Just gid ->
      case readMaybe gid of
        Just g -> pure g
        Nothing -> error $ "Could not read guild id " <> gid
    Nothing -> error "$GUILD_ID not set"

-- | Given the test server and an action operating on a channel id, get the
-- first text channel of that server and use the action on that channel.
actionWithChannelId :: DT.GuildId -> (DT.ChannelId -> D.DiscordHandler a) -> D.DiscordHandler a
actionWithChannelId testserverid f = do
  Right chans <- D.restCall $ R.GetGuildChannels testserverid
  (f . DT.channelId) (head (filter isTextChannel chans))
  where
    isTextChannel :: DT.Channel -> Bool
    isTextChannel DT.ChannelText {} = True
    isTextChannel _ = False
