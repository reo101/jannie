{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Schema (
  User (..),
  migrateAll,
  EntityField (..),
)
where

import Database.Persist.Class (PersistEntity (EntityField))
import Database.Persist.TH (
  MkPersistSettings (mpsPrefixFields),
  mkMigrate,
  mkPersist,
  persistLowerCase,
  share,
  sqlSettings,
 )
import User.FN qualified as User
import User.Name qualified as User

share
  [mkPersist sqlSettings {mpsPrefixFields = False}, mkMigrate "migrateAll"]
  [persistLowerCase|
User
    name User.Name
    fn User.FN
    deriving Show Eq Ord
    UniqueFN fn
|]
