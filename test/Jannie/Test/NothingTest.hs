module Jannie.Test.NothingTest where

import Hedgehog (TestT, (===))

test_nothing :: TestT IO ()
test_nothing = "jannie" === "jannie"
