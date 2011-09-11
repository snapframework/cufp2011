{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Snap.Chat.Internal.API.Tests (tests) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Attoparsec as A
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as Map
import           Data.Maybe (isJust)
import           Data.Text (Text)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import qualified Test.HUnit as H
import           Test.QuickCheck hiding (Success, Result, reason)
------------------------------------------------------------------------------
import           Snap.Chat.Test.Common
import           Snap.Chat.Internal.API.Types
import           Snap.Chat.Internal.Types

------------------------------------------------------------------------------
tests :: [Test]
tests = [ testApiRequestFromJson
        , testApiResponseToJson
        , testEncodedSessionJsonInvertible
        , testTrivialRequestsFromJson
        , testTrivialResponsesToJson
        , testWriteMessageRequestFromJson
        , testGetMessagesResponseToJson
        ]


------------------------------------------------------------------------------
instance (Arbitrary a) => Arbitrary (ApiResponse a) where
    arbitrary = do
        b <- arbitrary
        if b
          then ApiResponseSuccess <$> arbitrary <*> arbitrary
          else ApiResponseFailure <$> arbitrary <*> arbitrary

instance Arbitrary EncodedSession where
    arbitrary = EncodedSession            <$>
                (UserToken <$> arbitrary) <*>
                (toEnum    <$> arbitrary) <*>
                arbitrary

instance Arbitrary GetMessagesResponse where
    arbitrary = GetMessagesOK <$> arbitrary


------------------------------------------------------------------------------
testEncodedSessionJsonInvertible :: Test
testEncodedSessionJsonInvertible =
    testProperty "api/encodedSessionJsonInvertible" prop
  where
    prop :: EncodedSession -> Bool
    prop = propJSONInvertible


------------------------------------------------------------------------------
testApiRequestFromJson :: Test
testApiRequestFromJson = testCase "api/apiRequestFromJson" $ do
    let r = fromJSON $ fromRight $ A.parseOnly json jsonText
    case r of
      (Error e)   -> error e
      (Success x) -> do
         H.assertEqual "session ok" "abcdefg" $ _encodedSession x
         H.assertEqual "string ok" ("foo"::Text) $ _requestData x
  where
    jsonText = "{ \"session\": \"abcdefg\", \"requestData\": \"foo\" }"


------------------------------------------------------------------------------
testApiResponseToJson :: Test
testApiResponseToJson = testProperty "api/apiResponseFromJson" prop
  where
    prop :: ApiResponse Int -> Bool
    prop resp = p1 resp
      where
        p1 r@(ApiResponseSuccess session responseData) =
            let s          = S.concat $ L.toChunks $ encode r
                (Object j) = fromRight $ A.parseOnly json s
            in and [ check "session doesn't match" (== session)
                                                   (j .: "session")
                   , check "status not ok" (== ("ok"::Text)) (j .: "status")
                   , check "data not ok" (== responseData) (j .: "response")
                   ]

        p1 r@(ApiResponseFailure code reason) =
            let s          = S.concat $ L.toChunks $ encode r
                (Object j) = fromRight $ A.parseOnly json s
            in and [ check "status not failure" (== ("failure"::Text))
                                                (j .: "status")
                   , check "code not ok" (== code) (j .: "statusCode")
                   , check "reason not ok" (== reason) (j .: "reason")
                   ]


------------------------------------------------------------------------------
testTrivialRequestsFromJson :: Test
testTrivialRequestsFromJson = testCase "api/trivialRequestsFromJson" $ do
    let r = fromJSON $ fromRight $ A.parseOnly json jsonText
    case r of
      (Error e)   -> error e
      (Success x) -> do
         H.assertEqual "user ok" "fred" $ _desiredUserName x

    let r2 = fromJSON $ fromRight $ A.parseOnly json jsonText2
    case r2 of
      (Error e)   -> error e
      (Success x) -> do
         H.assertEqual "leave ok" LeaveRequest x

    let r3 = fromJSON $ fromRight $ A.parseOnly json jsonText2
    case r3 of
      (Error e)   -> error e
      (Success x) -> do
         H.assertEqual "GetMessagesRequest ok" GetMessagesRequest x

  where
    jsonText  = "{ \"desiredUserName\": \"fred\" }"
    jsonText2 = "{}"



------------------------------------------------------------------------------
testTrivialResponsesToJson :: Test
testTrivialResponsesToJson = testCase "api/trivialResponsesToJson" $ do
    let b = and [ prop JoinResponseOK
                , prop JoinResponseUserAlreadyExists
                , prop LeaveResponseOK
                , prop WriteMessageResponseOK
                ]
    H.assertBool "JoinResponse/LeaveResponse serialization should be empty map" b
  where
    prop :: ToJSON a => a -> Bool
    prop resp = j == Map.empty
      where
        s          = S.concat $ L.toChunks $ encode resp
        (Object j) = fromRight $ A.parseOnly json s


------------------------------------------------------------------------------
testWriteMessageRequestFromJson :: Test
testWriteMessageRequestFromJson = testCase "types/writeMessageFromJson" $ do
    H.assertEqual "talk"   (WriteMessageRequest contents1) value1
    H.assertEqual "action" (WriteMessageRequest contents2) value2
    H.assertEqual "join"   (WriteMessageRequest contents3) value3
    H.assertEqual "leave"  (WriteMessageRequest contents4) value4
    return ()

  where
    text1 = "foo-text-1"
    text2 = "foo-text-2"
    text4 = "foo-text-4"

    contents1 = Talk text1
    contents2 = Action text2
    contents3 = Join
    contents4 = Leave text4

    str1 = S.concat $ L.toChunks $ encode contents1
    str2 = S.concat $ L.toChunks $ encode contents2
    str3 = S.concat $ L.toChunks $ encode contents3
    str4 = S.concat $ L.toChunks $ encode contents4

    json1 = fromRight $ A.parseOnly json str1
    json2 = fromRight $ A.parseOnly json str2
    json3 = fromRight $ A.parseOnly json str3
    json4 = fromRight $ A.parseOnly json str4

    value1 = fromResult $ fromJSON json1
    value2 = fromResult $ fromJSON json2
    value3 = fromResult $ fromJSON json3
    value4 = fromResult $ fromJSON json4


------------------------------------------------------------------------------
testGetMessagesResponseToJson :: Test
testGetMessagesResponseToJson =
    testProperty "api/getMessagesResponseToJson" prop
  where
    prop :: GetMessagesResponse -> Bool
    prop resp@(GetMessagesOK msgs) =
        let s     = S.concat $ L.toChunks $ encode resp
            msgs' = fromResult $ fromJSON $ fromRight $ A.parseOnly json s
        in Just msgs == Map.lookup ("messages"::Text) msgs'


------------------------------------------------------------------------------
check :: String -> (a -> Bool) -> Parser a -> Bool
check s f p = isJust $ parseMaybe (const p') ()
  where
    p' = do
      v <- p
      if f v then return () else fail s


------------------------------------------------------------------------------
fromResult :: Result a -> a
fromResult (Error e) = error e
fromResult (Success s) = s

