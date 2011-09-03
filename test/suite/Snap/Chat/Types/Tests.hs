{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Snap.Chat.Types.Tests (tests) where

------------------------------------------------------------------------------
import           Control.Monad
import           Data.Aeson
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
------------------------------------------------------------------------------
import           Snap.Chat.Test.Common
import           Snap.Chat.Internal.Types

------------------------------------------------------------------------------
tests :: [Test]
tests = [ testMessageJsonInstance
        , testTrivials
        ]


------------------------------------------------------------------------------
testMessageJsonInstance :: Test
testMessageJsonInstance = testProperty "types/messageJsonInstance" prop
  where
    prop :: Message -> Bool
    prop msg = case result of
                 Error _      -> False
                 Success msg' -> msg == msg'
      where
        result = fromJSON $ toJSON msg


testTrivials :: Test
testTrivials = testCase "types/trivials" $ do
    mapM_ coverEqInstance   contents
    mapM_ coverShowInstance contents
    mapM_ coverEqInstance   messages
    mapM_ coverShowInstance messages
  where
    contents = [ Talk ""
               , Action ""
               , Join
               , Leave ""
               ]

    messages = map (Message "" 0) contents
