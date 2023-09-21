module Jannie.Test.NameTest where

import Hedgehog (TestT, (===))

import Jannie (botName)

test_name :: TestT IO ()
test_name = "jannie" === botName
