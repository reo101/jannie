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
  = Run
      { discordConfigFile :: w ::: Maybe String <?> "File containing discord configuration for Jannie. The config can be overridden through env variables."
      }
  deriving stock (Generic)

readCommand :: IO Command
readCommand = unwrapRecord "jannie"

type Command = Command' Unwrapped

deriving anyclass instance ParseRecord (Command' Wrapped)
