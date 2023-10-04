module Utils (
  showText,
  whenLeft,
  (<$$>),
)
where

import Data.Text (Text)
import Data.Text qualified as Text

showText :: (Show a) => a -> Text
showText = Text.pack . show

whenLeft :: (Applicative m) => Either a b -> (a -> m b) -> m b
whenLeft (Left e) f = f e
whenLeft (Right x) _ = pure x

(<$$>) :: (Functor f0) => (Functor f1) => (a -> b) -> f0 (f1 a) -> f0 (f1 b)
f <$$> mmx = fmap (fmap f) mmx
