{-# LANGUAGE NoFieldSelectors #-}

module User.Name (
  Name (get),
  parse,
  regexPattern,
) where

import Data.Text (Text)
import Database.Persist.Class.PersistField
import Database.Persist.Sql (PersistFieldSql)
import Text.Regex.TDFA ((=~~))

newtype Name = MkName {get :: Text}
  deriving newtype
    ( Show
    , Eq
    , Ord
    , PersistField
    , PersistFieldSql
    )

parse :: Text -> Maybe Name
parse text =
  MkName <$> text =~~ regexPattern

regexPattern :: String
regexPattern = "^[А-Я][а-я]+ [А-Я][а-я]+$"
