module Utils where

import qualified Data.Text as T
import Discord
import qualified Discord.Requests as R
import Discord.Types
import Text.Read (readMaybe)
import System.Environment (lookupEnv)

getToken :: IO T.Text
getToken = do
  maybeToken <- lookupEnv "AUTH_TOKEN"
  case maybeToken of
    Just token -> pure $ T.pack token
    Nothing -> error "$AUTH_TOKEN not set"

getGuildId :: IO GuildId
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
actionWithChannelId :: GuildId -> (ChannelId -> DiscordHandler a) -> DiscordHandler a
actionWithChannelId testserverid f = do
  Right chans <- restCall $ R.GetGuildChannels testserverid
  (f . channelId) (head (filter isTextChannel chans))
  where
    isTextChannel :: Channel -> Bool
    isTextChannel ChannelText {} = True
    isTextChannel _ = False
