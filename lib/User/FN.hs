module User.FN (
  FN (get),
  parse,
  regexPattern,
) where

import Data.Text (Text)
import Text.Regex.TDFA ((=~~))

newtype FN = MkFN {get :: Text}

parse :: Text -> Maybe FN
parse text =
  MkFN <$> text =~~ regexPattern

regexPattern :: String
regexPattern = "^([0-9]{5}|[0-9]MI[0-9]{7})$"
