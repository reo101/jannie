module User.FN (
  FN (get),
  parse,
  regexPattern,
) where

import Data.Text (Text)
import Database.Persist.Class.PersistField (PersistField)
import Database.Persist.Sql (PersistFieldSql)
import Text.Regex.TDFA ((=~~))

newtype FN = MkFN {get :: Text}
  deriving newtype
    ( Show
    , Eq
    , Ord
    , PersistField
    , PersistFieldSql
    )

parse :: Text -> Maybe FN
parse text =
  MkFN <$> text =~~ regexPattern

regexPattern :: String
regexPattern = "^([0-9]{5}|[0-9]MI[0-9]{7})$"
