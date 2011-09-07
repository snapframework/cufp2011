{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Snap.Chat.ChatRoom
  ( newChatRoom
  , destroyChatRoom
  , withChatRoom
  , joinUser
  , authenticateUser
  , lookupUser
  , disconnectUser
  , getMessages
  , writeMessage
  , writeMessageContents

    -- * Exceptions
  , UserAlreadyConnectedException
  , UserNotConnectedException
  , UserAuthenticationFailureException
  ) where


------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString as S
import qualified Data.ByteString.Base16 as B16
import qualified Data.HashTable.IO as HT
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable
import           Data.Word (Word8)
import           System.PosixCompat.Time
import           System.Random.MWC
import           System.Timeout
------------------------------------------------------------------------------
import qualified Snap.Chat.Message as Msg
import           Snap.Chat.Internal.Types
import           Snap.Chat.Internal.Util
import qualified System.TimeoutManager as TM


------------------------------------------------------------------------------
data UserAlreadyConnectedException = UserAlreadyConnectedException UserName
  deriving (Typeable)

instance Show UserAlreadyConnectedException where
  show (UserAlreadyConnectedException u) =
      concat [ "User \""
             , T.unpack u
             , "\" already connected." ]

instance Exception UserAlreadyConnectedException


------------------------------------------------------------------------------
data UserNotConnectedException = UserNotConnectedException UserName
  deriving (Typeable)

instance Show UserNotConnectedException where
  show (UserNotConnectedException u) =
      concat [ "User \""
             , T.unpack u
             , "\" not connected." ]

instance Exception UserNotConnectedException


------------------------------------------------------------------------------
data UserAuthenticationFailureException =
    UserAuthenticationFailureException UserName
  deriving (Typeable)

instance Show UserAuthenticationFailureException where
  show (UserAuthenticationFailureException u) =
      concat [ "Authentication failed for user \""
             , T.unpack u
             , "\"." ]

instance Exception UserAuthenticationFailureException


------------------------------------------------------------------------------
newChatRoom :: Int -> IO ChatRoom
newChatRoom userTimeout =
    ChatRoom                              <$>
      TM.initialize userTimeout epochTime <*>
      (HT.new >>= newMVar)                <*>
      atomically newTChan                 <*>
      pure userTimeout



------------------------------------------------------------------------------
destroyChatRoom :: ChatRoom -> IO ()
destroyChatRoom = TM.stop . _timeoutManager


------------------------------------------------------------------------------
withChatRoom :: Int -> (ChatRoom -> IO a) -> IO a
withChatRoom userTimeout = bracket (newChatRoom userTimeout) destroyChatRoom


------------------------------------------------------------------------------
-- | Connect a new user to the chat room. Throws UserAlreadyConnectedException
--   if the user was already connected.
joinUser :: Text -> ChatRoom -> IO User
joinUser userName chatRoom = withMVar userMapMVar $ \userMap -> do
    HT.lookup userMap userName >>=
      maybe (return ())
            (const $ throwIO $ UserAlreadyConnectedException userName)

    user <- User                                  <$>
              pure userName                       <*>
              (atomically $ dupTChan chatChannel) <*>
              makeUserToken                       <*>
              TM.register (disconnectUser userName
                                          disconnectionMessage
                                          chatRoom)
                          timeoutManager

    HT.insert userMap userName user
    joinMsg  <- Msg.join user
    writeMessage joinMsg user chatRoom
    return user

  where
    disconnectionMessage = T.concat [ " has left the channel (timeout). "
                                    ]

    timeoutManager = _timeoutManager chatRoom
    userMapMVar    = _userMap chatRoom
    chatChannel    = _chatChannel chatRoom


------------------------------------------------------------------------------
makeUserToken :: IO UserToken
makeUserToken = withSystemRandom $ \gen -> do
    xs <- (replicateM 16 $ uniform gen) :: IO [Word8]
    return $ UserToken $ B16.encode $ S.pack xs


------------------------------------------------------------------------------
disconnectUser :: UserName -> Text -> ChatRoom -> IO ()
disconnectUser userName disconnectionReason chatRoom =
    withMVar userMapMVar $ \userMap ->
        HT.lookup userMap userName >>= maybe (return ()) (destroy userMap)

  where
    userMapMVar    = _userMap chatRoom

    destroy userMap user = do
        leaveMsg <- Msg.leave disconnectionReason user
        writeMessage leaveMsg user chatRoom
        TM.cancel $ _timeoutHandle user
        HT.delete userMap userName


------------------------------------------------------------------------------
lookupUser :: UserName -> ChatRoom -> IO (Maybe User)
lookupUser userName chatRoom = withMVar userMapMVar $ flip HT.lookup userName
  where
    userMapMVar    = _userMap chatRoom


------------------------------------------------------------------------------
authenticateUser :: UserName -> UserToken -> ChatRoom -> IO User
authenticateUser userName userToken chatRoom =
    withMVar userMapMVar authenticate
  where
    userMapMVar    = _userMap chatRoom

    authenticate userMap = do
        mbU <- HT.lookup userMap userName
        maybe (throwIO $ UserNotConnectedException userName)
              (\user ->
                   if getUserToken user /= userToken
                     then throwIO $ UserAuthenticationFailureException userName
                     else return user)
              mbU


------------------------------------------------------------------------------
-- | Get new messages posted to the channel for the given connected user. If no
-- messages are available, blocks for the given number of seconds, returning an
-- empty list if the timeout expires.
getMessages :: Int -> User -> ChatRoom -> IO [Message]
getMessages timeoutInSeconds (User _ chan _ timeoutHandle) cr = do
    TM.tickle timeoutHandle userTimeout
    xs <- readAllFromChan timeoutInSeconds chan
    TM.tickle timeoutHandle userTimeout
    return xs

  where
    userTimeout = _userTimeout cr



------------------------------------------------------------------------------
-- | Write a message to the channel.
writeMessage :: Message -> User -> ChatRoom -> IO ()
writeMessage msg user cr = do
    atomically $ writeTChan chan msg
    _ <- readAllFromChan 1 chan
    TM.tickle timeoutHandle userTimeout

  where
    chan          = _chatChannel cr
    timeoutHandle = _timeoutHandle user
    userTimeout = _userTimeout cr


------------------------------------------------------------------------------
-- | Write a message to the channel.
writeMessageContents :: MessageContents -> User -> ChatRoom -> IO ()
writeMessageContents msgContents user cr = do
    now <- epochTime
    let userName = _userName user
    writeMessage (Message userName now msgContents) user cr



------------------------------------------------------------------------------
readAllFromChan :: Int -> TChan a -> IO [a]
readAllFromChan secondsToWait chan = do
    m <- timeout (seconds secondsToWait) $ atomically readAll
    return $ fromMaybe [] m
  where
    readAll = do
        v <- readTChan chan
        readRest (v:)

    readRest !dlist = do
        done <- isEmptyTChan chan

        if done
          then return $! dlist []
          else do
              v <- readTChan chan
              readRest (dlist . (v:))
{-# INLINE readAllFromChan #-}
