module Config where

import qualified Data.Text as T
import qualified Discord as D
import qualified Discord.Requests as R
import qualified Discord.Types as DT
import Text.Read (readMaybe)
import System.Environment (lookupEnv)
import qualified Discord.Internal.Types.Prelude as DITP

data Config = Config
  { guildId :: DT.GuildId
  , adminRoles :: [DT.RoleId]
  }
