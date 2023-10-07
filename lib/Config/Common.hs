{-# LANGUAGE NoFieldSelectors #-}

module Config.Common (
  ParseEnvResult (..),
  handleParseEnvResultMaybe,
  handleParseEnvResultThrow,
  readEnvText,
  readEnv,
)
where

import Control.Exception (throwIO)
import Data.Text (Text)
import Data.Text qualified as Text
import System.Environment (lookupEnv)
import Text.Printf (printf)
import Text.Read (readEither)

data ParseEnvResult e a
  = MissingKey String
  | CouldNotParse {key :: String, rawValue :: String, err :: e}
  | Success a
  deriving stock (Functor)

parseEnv :: (String -> Either e a) -> String -> IO (ParseEnvResult e a)
parseEnv parse key = do
  maybeRawValue <- lookupEnv key
  pure case maybeRawValue of
    Nothing -> MissingKey key
    Just rawValue ->
      case parse rawValue of
        Left err -> CouldNotParse key rawValue err
        Right x -> Success x

readEnvText :: String -> IO (ParseEnvResult String Text)
readEnvText = parseEnv (Right . Text.pack)

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
