{-# LANGUAGE ApplicativeDo #-}

module Config.Db (
  Config (..),
  getConfig,
  toConnString,
)
where

import Config.Common (
  handleParseEnvResultMaybe,
  handleParseEnvResultThrow,
  readEnvText,
 )
import Control.Exception (throwIO)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (FromJSON)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString
import Data.Maybe (fromMaybe)
import Data.Text
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Text.Printf (printf)
import Utils (whenLeft)

data Config = MkConfig
  { user :: Text
  , password :: Text
  , host :: Text
  , port :: Text
  , database :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

-- TODO: rename
getConfig :: Maybe FilePath -> IO Config
getConfig configFile = do
  user <- readEnvText "PGUSER"
  password <- readEnvText "PGPASSWORD"
  host <- readEnvText "PGHOST"
  port <- readEnvText "PGPORT"
  database <- readEnvText "PGDATABASE"
  let parseThrow x = do
        parsed <- Aeson.eitherDecodeFileStrict x
        whenLeft parsed $ \err ->
          throwIO . userError $ "Failed to parse config file: " <> err
  configFromFileMay :: Maybe Config <- traverse parseThrow configFile
  case configFromFileMay of
    Nothing -> do
      user <- handleParseEnvResultThrow user
      password <- handleParseEnvResultThrow password
      host <- handleParseEnvResultThrow host
      port <- handleParseEnvResultThrow port
      database <- handleParseEnvResultThrow database
      pure MkConfig {..}
    Just configFromFile -> do
      userMay <- handleParseEnvResultMaybe user
      passwordMay <- handleParseEnvResultMaybe password
      hostMay <- handleParseEnvResultMaybe host
      portMay <- handleParseEnvResultMaybe port
      databaseMay <- handleParseEnvResultMaybe database
      pure
        MkConfig
          { user = fromMaybe configFromFile.user userMay
          , password = fromMaybe configFromFile.password passwordMay
          , host = fromMaybe configFromFile.host hostMay
          , port = fromMaybe configFromFile.port portMay
          , database = fromMaybe configFromFile.database databaseMay
          }

toConnString :: Config -> ByteString
toConnString MkConfig {..} =
  ByteString.pack $
    printf
      "host=%s port=%s user=%s dbname=%s password=%s"
      (Text.unpack host)
      (Text.unpack port)
      (Text.unpack user)
      (Text.unpack database)
      (Text.unpack password)
