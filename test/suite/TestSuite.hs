module Main where

import           Test.Framework (defaultMain, testGroup)

import qualified Snap.Chat.ChatRoom.Tests
import qualified Snap.Chat.Internal.API.Tests
import qualified Snap.Chat.Types.Tests

main :: IO ()
main = defaultMain tests
  where
    tests = [
        testGroup "Snap.Chat.ChatRoom.Tests" Snap.Chat.ChatRoom.Tests.tests
      , testGroup "Snap.Chat.Internal.API.Tests"
                  Snap.Chat.Internal.API.Tests.tests
      , testGroup "Snap.Chat.Types.Tests"    Snap.Chat.Types.Tests.tests
      ]
