{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Snap.Chat.Types.Tests (tests) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Attoparsec as A
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Text (Text)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
------------------------------------------------------------------------------
import           Snap.Chat.Test.Common
import           Snap.Chat.Internal.Types

------------------------------------------------------------------------------
tests :: [Test]
tests = [ testMessageJsonInvertible
        , testMessageJsonInstance
        , testTrivials
        ]


------------------------------------------------------------------------------
testMessageJsonInvertible :: Test
testMessageJsonInvertible = testProperty "types/messageJsonInvertible" prop
  where
    prop :: Message -> Bool
    prop = propJSONInvertible


------------------------------------------------------------------------------
testMessageJsonInstance :: Test
testMessageJsonInstance = testCase "types/messageJsonInstance" $ do
    !_ <- return $! res1
    !_ <- return $! res2
    !_ <- return $! res3
    !_ <- return $! res4
    return ()

  where
    text1 = "foo-text-1"
    text2 = "foo-text-2"
    text4 = "foo-text-4"

    contents1 = Talk text1
    contents2 = Action text2
    contents3 = Join
    contents4 = Leave text4

    tm = 12345678

    msg1 = Message "user1" tm contents1
    msg2 = Message "user2" tm contents2
    msg3 = Message "user3" tm contents3
    msg4 = Message "user4" tm contents4

    str1 = S.concat $ L.toChunks $ encode msg1
    str2 = S.concat $ L.toChunks $ encode msg2
    str3 = S.concat $ L.toChunks $ encode msg3
    str4 = S.concat $ L.toChunks $ encode msg4

    (Object json1) = fromRight $ A.parseOnly json str1
    (Object json2) = fromRight $ A.parseOnly json str2
    (Object json3) = fromRight $ A.parseOnly json str3
    (Object json4) = fromRight $ A.parseOnly json str4

    check :: String -> (a -> Bool) -> Parser a -> Parser ()
    check s f p = do
        v <- p
        if f v then return () else fail s

    p1 = do
        check "user not user1" (==("user1"::Text)) $ json1 .: "user"
        check "time doesn't match" (==tm) (toEnum <$> json1 .: "time")
        contents <- json1 .: "contents"
        check "type isn't talk" (==("talk"::Text)) $ contents .: "type"
        check "text doesn't match" (==text1) $ contents .: "text"
        return ()

    p2 = do
        check "user not user2" (==("user2"::Text)) $ json2 .: "user"
        check "time doesn't match" (==tm) (toEnum <$> json2 .: "time")
        contents <- json2 .: "contents"
        check "type isn't action" (==("action"::Text)) $ contents .: "type"
        check "text doesn't match" (==text2) $ contents .: "text"
        return ()

    p3 = do
        check "user not user3" (==("user3"::Text)) $ json3 .: "user"
        check "time doesn't match" (==tm) (toEnum <$> json3 .: "time")
        contents <- json3 .: "contents"
        check "type isn't join" (==("join"::Text)) $ contents .: "type"
        return ()

    p4 = do
        check "user not user4" (==("user4"::Text)) $ json4 .: "user"
        check "time doesn't match" (==tm) (toEnum <$> json4 .: "time")
        contents <- json4 .: "contents"
        check "type isn't leave" (==("leave"::Text)) $ contents .: "type"
        check "text doesn't match" (==text4) $ contents .: "text"
        return ()

    res1 = fromRight $ parseEither (const p1) ()
    res2 = fromRight $ parseEither (const p2) ()
    res3 = fromRight $ parseEither (const p3) ()
    res4 = fromRight $ parseEither (const p4) ()


------------------------------------------------------------------------------
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
