{-# LANGUAGE NoFieldSelectors #-}

module Command (
  Command' (..),
  Command,
  readCommand,
)
where

import GHC.Generics (Generic)
import Options.Generic (ParseRecord, Unwrapped, Wrapped, unwrapRecord, (:::), type (<?>))

data Command' w
  = Migrate
      { forReal :: w ::: Bool <?> "By default we only print migrations. Providing this will actually run them"
      , dbConfigFile :: w ::: Maybe String <?> DbConfigFileHelp
      }
  | Run
      { discordConfigFile :: w ::: Maybe String <?> "File containing discord configuration for Jannie. The config can be overridden through env variables."
      , dbConfigFile :: w ::: Maybe String <?> DbConfigFileHelp
      }
  deriving stock (Generic)

readCommand :: IO Command
readCommand = unwrapRecord "jannie"

type Command = Command' Unwrapped

type DbConfigFileHelp = "File containing config for PG. The config can be overridden through env variables."

deriving anyclass instance ParseRecord (Command' Wrapped)
