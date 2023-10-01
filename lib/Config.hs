{-# LANGUAGE NamedFieldPuns #-}
module Config where

import qualified Discord.Types as DT
import qualified Data.Text as T
import Text.Read (readMaybe)
import System.Environment (lookupEnv)
import Text.Printf (printf)

data Config = Config
  { token :: T.Text
  , guildId :: DT.GuildId
  -- , adminRoles :: [DT.RoleId]
  , defaultRoles :: [DT.RoleId]
  }

parseEnv :: Read a => (String -> Maybe a) -> String -> IO a
parseEnv parser key = do
  maybeRawValue <- lookupEnv key
  case maybeRawValue of 
    Nothing -> error $ printf "$%s not set" key
    Just rawValue -> case parser rawValue of
      Nothing -> error $ printf "$%s could not be parsed: %s" key rawValue
      Just value -> pure value

readEnv :: Read a => String -> IO a
readEnv = parseEnv readMaybe

getConfig :: IO Config
getConfig = do
  token <- parseEnv (Just . T.pack) "AUTH_TOKEN"
  guildId <- readEnv "GUILD_ID"
  -- adminRoles <- readEnv "ADMIN_ROLES"
  defaultRoles <- readEnv "DEFAULT_ROLES"

  pure $ 
    Config 
      { token
      , guildId
      -- , adminRoles
      , defaultRoles
      }