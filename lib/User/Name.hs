{-# LANGUAGE NoFieldSelectors #-}

module User.Name (
  Name (get),
  parse,
  regexPattern,
) where

import Data.Text (Text)
import Text.Regex.TDFA ((=~~))

newtype Name = MkName {get :: Text}

parse :: Text -> Maybe Name
parse text =
  MkName <$> text =~~ regexPattern

regexPattern :: String
regexPattern = "^[А-Я][а-я]+ [А-Я][а-я]+$"
