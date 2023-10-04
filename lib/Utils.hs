module Utils (
  showText,
)
where

import Data.Text (Text)
import Data.Text qualified as Text

showText :: (Show a) => a -> Text
showText = Text.pack . show
