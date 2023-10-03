module Args (
  Args' (..),
  Args,
  readArgs,
)
where

import GHC.Generics (Generic)
import Options.Generic (ParseRecord, Unwrapped, Wrapped, unwrapRecord, (:::), type (<?>))

data Args' w = MkArgs
  { configFile :: w ::: Maybe String <?> "File containing configuration for Jannie. The config can be overridden through env variables."
  }
  deriving stock (Generic)

readArgs :: IO Args
readArgs = unwrapRecord "jannie"

type Args = Args' Unwrapped

deriving anyclass instance ParseRecord (Args' Wrapped)
