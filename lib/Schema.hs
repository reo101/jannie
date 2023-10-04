{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Schema (
  User (..),
  migrateAll,
)
where

import Database.Persist.TH
import User.FN qualified as User
import User.Name qualified as User

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
User
    name User.Name
    fn User.FN
    deriving Show Eq Ord
    UniqueFN fn
|]
