{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Snap.Chat.ChatRoom.Tests (tests) where

------------------------------------------------------------------------------
import           Control.Concurrent
import qualified Data.ByteString.Char8 as S
import           System.PosixCompat.Time
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (Test)
------------------------------------------------------------------------------
import           Snap.Chat.ChatRoom
import           Snap.Chat.Internal.Util
import           Snap.Chat.Test.Common
import           Snap.Chat.Internal.Types

------------------------------------------------------------------------------
tests :: [Test]
tests = [ testConnectAndLookup
        , testUserTimeout
        , testConnectTwice
        , testAuthenticate
        ]


------------------------------------------------------------------------------
testConnectAndLookup :: Test
testConnectAndLookup = testCase "chatroom/connectAndLookup" $
                       withChatRoom 10 proc
  where
    ------------------------------------------------------------------------
    userName  = "cufp2011"
    userName2 = "secondUser"

    ------------------------------------------------------------------------
    isJoin :: MessageContents -> Bool
    isJoin Join = True
    isJoin _    = False


    ------------------------------------------------------------------------
    isLeave :: MessageContents -> Bool
    isLeave (Leave x) = x `seq` True
    isLeave _         = False


    ------------------------------------------------------------------------
    proc chatRoom = do
        now  <- epochTime
        user <- joinUser userName chatRoom

        lookupUser userName chatRoom >>= 
            maybe (assertBool "user not found" False)
                  (\u2 -> do
                       assertEqual "usernames don't match"
                                   userName
                                   (getUserName u2))
              
        -- only message on the channel should be the join message
        msgs <- getMessages 1 user chatRoom
        assertEqual "only one message on channel" 1 $ length msgs
        let msg = head msgs

        assertEqual "message user doesn't match"
                    userName
                    (getMessageUserName msg)

        let timeDelta = getMessageTime msg - now
        assertBool "message time mismatch" (abs timeDelta <= 2)
        assertBool "message is a join" $ isJoin $ getMessageContents msg

        user2 <- joinUser userName2 chatRoom
        disconnectUser userName "goodbye" chatRoom
        lookupUser userName chatRoom >>= 
            maybe (return ())
                  (\_ -> assertBool "user should be gone" False)
        msgs2 <- getMessages 1 user2 chatRoom
        assertEqual "two messages on channel" 2 $ length msgs2
        let [joinMsg, leaveMsg] = msgs2

        assertEqual "message user doesn't match"
                    userName2
                    (getMessageUserName joinMsg)
        assertBool "message is a join" $ isJoin $ getMessageContents joinMsg

        assertEqual "message user doesn't match"
                    userName
                    (getMessageUserName leaveMsg)
        assertBool "message is a leave" $ isLeave $ getMessageContents leaveMsg


------------------------------------------------------------------------------
testConnectTwice :: Test
testConnectTwice = testCase "chatroom/connectTwice" $
                   withChatRoom 10 proc
  where
    ------------------------------------------------------------------------
    userName = "cufp2011"

    ------------------------------------------------------------------------
    proc chatRoom = do
        _ <- joinUser userName chatRoom
        expectExceptionH $ joinUser userName chatRoom


------------------------------------------------------------------------------
testUserTimeout :: Test
testUserTimeout = testCase "chatroom/userTimeout" $
                  withChatRoom 1 proc
  where
    ------------------------------------------------------------------------
    userName = "cufp2011"

    ------------------------------------------------------------------------
    proc chatRoom = do
        _ <- joinUser userName chatRoom
        threadDelay $ seconds 3

        lookupUser userName chatRoom >>= 
            maybe (return ())
                  (\_ -> assertBool "user didn't timeout" False)


------------------------------------------------------------------------------
testAuthenticate :: Test
testAuthenticate = testCase "chatroom/authenticate" $
                   withChatRoom 10 proc
  where
    ------------------------------------------------------------------------
    userName  = "cufp2011"
    userName2 = "junk"

    ------------------------------------------------------------------------
    proc chatRoom = do
        user <- joinUser userName chatRoom

        let oldToken               = getUserToken user
        let (UserToken oldTokenBS) = oldToken
        let newToken               = UserToken $ S.drop 1 oldTokenBS

        expectExceptionH $ authenticateUser userName newToken chatRoom
        expectExceptionH $ authenticateUser userName2 oldToken chatRoom
        user' <- authenticateUser userName oldToken chatRoom

        -- expect the token to have changed.
        assertBool "token didn't change" $ getUserToken user' /= oldToken
