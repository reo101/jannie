{-# LANGUAGE NoFieldSelectors #-}

module Config (
  Config (..),
  getConfig,
) where

import Control.Exception (throwIO)
import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Discord.Types qualified as DT
import GHC.Generics (Generic)
import System.Environment (lookupEnv)
import Text.Printf (printf)
import Text.Read (readEither)

data Config = Config
  { token :: T.Text
  , guildId :: DT.GuildId
  , defaultRoles :: [DT.RoleId]
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

getConfig :: Maybe FilePath -> IO Config
getConfig configFile = do
  token <- parseEnv (Right . T.pack) "AUTH_TOKEN"
  guildId <- readEnv "GUILD_ID"
  defaultRoles <- readEnv "DEFAULT_ROLES"
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
      pure Config {..}
    Just configFromFile -> do
      tokenMay <- handleParseEnvResultMaybe token
      guildIdMay <- handleParseEnvResultMaybe guildId
      defaultRolesMay <- handleParseEnvResultMaybe defaultRoles
      pure
        Config
          { token = fromMaybe configFromFile.token tokenMay
          , guildId = fromMaybe configFromFile.guildId guildIdMay
          , defaultRoles = fromMaybe configFromFile.defaultRoles defaultRolesMay
          }

data ParseEnvResult e a
  = MissingKey String
  | CouldNotParse {key :: String, rawValue :: String, err :: e}
  | Success a

parseEnv :: (String -> Either e a) -> String -> IO (ParseEnvResult e a)
parseEnv parse key = do
  maybeRawValue <- lookupEnv key
  pure case maybeRawValue of
    Nothing -> MissingKey key
    Just rawValue ->
      case parse rawValue of
        Left err -> CouldNotParse key rawValue err
        Right x -> Success x

readEnv :: (Read a) => String -> IO (ParseEnvResult String a)
readEnv = parseEnv readEither

-- | Throw on parse failures, Nothing on missing keys.
handleParseEnvResultMaybe :: ParseEnvResult String a -> IO (Maybe a)
handleParseEnvResultMaybe = \case
  MissingKey _key ->
    pure Nothing
  CouldNotParse {key, rawValue, err} ->
    throwIO . userError $ formatParseError key rawValue err
  Success x -> pure $ Just x

-- | Throw on any errors.
handleParseEnvResultThrow :: ParseEnvResult String a -> IO a
handleParseEnvResultThrow = \case
  MissingKey key ->
    throwIO . userError $ printf "$%s not set" key
  CouldNotParse {key, rawValue, err} ->
    throwIO . userError $ formatParseError key rawValue err
  Success x -> pure x

formatParseError :: String -> String -> String -> String
formatParseError key rawValue err =
  printf "$%s=%s could not be parsed: %s" key rawValue err

whenLeft :: (Applicative m) => Either a b -> (a -> m b) -> m b
whenLeft (Left e) f = f e
whenLeft (Right x) _ = pure x
