module App (
  discordCall,
) where

import Discord (DiscordHandler, FromJSON, Request, RestCallErrorCode, restCall)

discordCall :: (Discord.Request (r a), FromJSON a) => r a -> DiscordHandler (Either Discord.RestCallErrorCode a)
discordCall = Discord.restCall
