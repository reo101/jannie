module App (
  App,
  discordCall,
  runDb,
  liftDb,
  liftDiscord,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT (..), mapReaderT)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Aeson
import Data.Pool (Pool)
import Database.Persist.Sql (runSqlPool)
import Database.Persist.SqlBackend (SqlBackend)
import Discord (DiscordHandler, Request, RestCallErrorCode, restCall)
import Discord.Handle (DiscordHandle)
import UnliftIO (MonadUnliftIO)

newtype App a = MkApp {get :: ReaderT SqlBackend (ReaderT DiscordHandle IO) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

discordCall :: (Discord.Request (r a), FromJSON a) => r a -> App (Either Discord.RestCallErrorCode a)
discordCall = liftDiscord . Discord.restCall

runDb :: Pool SqlBackend -> App a -> DiscordHandler a
runDb pool app = runSqlPool app.get pool

liftDb :: ReaderT SqlBackend IO a -> App a
liftDb = MkApp . mapReaderT liftIO

liftDiscord :: DiscordHandler a -> App a
liftDiscord = MkApp . lift
