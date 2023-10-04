{-# LANGUAGE NoFieldSelectors #-}

module Config.Discord (
  Config (..),
  AuthToken (get),
  fromFileAndEnv,
) where

import Config.Common (
  handleParseEnvResultMaybe,
  handleParseEnvResultThrow,
  readEnv,
  readEnvText,
 )
import Control.Exception (throwIO)
import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Discord.Types qualified as DT
import GHC.Generics (Generic)
import Utils (whenLeft, (<$$>))

newtype AuthToken = MkAuthToken {get :: Text}
  deriving newtype (FromJSON)

data Config = Config
  { token :: AuthToken
  , guildId :: DT.GuildId
  , defaultRoles :: [DT.RoleId]
  , rootRole :: DT.RoleId
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

-- TODO: rename
fromFileAndEnv :: Maybe FilePath -> IO Config
fromFileAndEnv configFile = do
  token <- MkAuthToken <$$> readEnvText "AUTH_TOKEN"
  guildId <- readEnv "GUILD_ID"
  defaultRoles <- readEnv "DEFAULT_ROLES"
  rootRole <- readEnv "ROOT_ROLE"
  let parseThrow x = do
        parsed <- Aeson.eitherDecodeFileStrict x
        whenLeft parsed $ \err ->
          throwIO . userError $ "Failed to parse config file: " <> err
  configFromFileMay :: Maybe Config <- traverse parseThrow configFile
  case configFromFileMay of
    Nothing -> do
      token <- handleParseEnvResultThrow token
      guildId <- handleParseEnvResultThrow guildId
      defaultRoles <- handleParseEnvResultThrow defaultRoles
      rootRole <- handleParseEnvResultThrow rootRole
      pure Config {..}
    Just configFromFile -> do
      tokenMay <- handleParseEnvResultMaybe token
      guildIdMay <- handleParseEnvResultMaybe guildId
      defaultRolesMay <- handleParseEnvResultMaybe defaultRoles
      rootRoleMay <- handleParseEnvResultMaybe rootRole
      pure
        Config
          { token = fromMaybe configFromFile.token tokenMay
          , guildId = fromMaybe configFromFile.guildId guildIdMay
          , defaultRoles = fromMaybe configFromFile.defaultRoles defaultRolesMay
          , rootRole = fromMaybe configFromFile.rootRole rootRoleMay
          }
