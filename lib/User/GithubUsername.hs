module User.GithubUsername (
  GithubUsername (get),
  parse,
  regexPattern,
)
where

import Data.Text (Text)
import Text.Regex.TDFA ((=~~))

newtype GithubUsername = MkGithubUsername {get :: Text}

parse :: Text -> Maybe GithubUsername
parse text =
  MkGithubUsername <$> text =~~ regexPattern

regexPattern :: String
regexPattern = "^[a-zA-Z0-9]([-a-zA-Z0-9]{0,38}[a-zA-Z0-9])?$"
