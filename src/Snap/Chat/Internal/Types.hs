{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Snap.Chat.Internal.Types where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Data.Aeson
import qualified Data.Aeson.Types as A
import           Data.ByteString (ByteString)
import           Data.Data
import qualified Data.HashTable.IO as HT
import qualified Data.Map as Map
import           Data.Monoid
import           Data.Text (Text)
import           System.Posix.Types
------------------------------------------------------------------------------
import           System.TimeoutManager (TimeoutManager, TimeoutHandle)


------------------------------------------------------------------------------
toBeImplemented :: a
toBeImplemented = error "*** TODO: implement this ***"


------------------------------------------------------------------------------
type UserName = Text


------------------------------------------------------------------------------
data MessageContents = Talk   { _messageText :: !Text }
                     | Action { _messageText :: !Text }
                     | Join
                     | Leave  { _messageText :: !Text }
  deriving (Show, Eq)


instance FromJSON MessageContents where
    parseJSON (Object obj) = toBeImplemented
    parseJSON _ = fail "MessageContents: JSON object of wrong type"


------------------------------------------------------------------------------
instance ToJSON MessageContents where
    toJSON (Talk t)   = toBeImplemented
    toJSON (Action t) = toBeImplemented
    toJSON (Join)     = toBeImplemented
    toJSON (Leave t)  = toBeImplemented


------------------------------------------------------------------------------
data Message = Message {
      _messageUser     :: !UserName
    , _messageTime     :: !EpochTime
    , _messageContents :: !MessageContents
    }
  deriving (Show, Eq)


------------------------------------------------------------------------------
getMessageUserName :: Message -> UserName
getMessageUserName = _messageUser

getMessageTime :: Message -> EpochTime
getMessageTime = _messageTime

getMessageContents :: Message -> MessageContents
getMessageContents = _messageContents


------------------------------------------------------------------------------
instance FromJSON Message where
    parseJSON (Object obj) = toBeImplemented
    parseJSON _ = fail "Message: JSON object of wrong type"

instance ToJSON Message where
    toJSON (Message u t c) = toBeImplemented


------------------------------------------------------------------------------
newtype UserToken = UserToken ByteString
  deriving (Show, Eq, Data, Ord, Typeable, Monoid, FromJSON, ToJSON)


------------------------------------------------------------------------------
data User = User {
      _userName      :: !UserName
    , _userMsgChan   :: !(TChan Message)
    , _userToken     :: !UserToken
    , _timeoutHandle :: !TimeoutHandle
}


------------------------------------------------------------------------------
getUserName :: User -> UserName
getUserName = _userName


------------------------------------------------------------------------------
getUserToken :: User -> UserToken
getUserToken = _userToken


------------------------------------------------------------------------------
type HashTable k v = HT.CuckooHashTable k v


------------------------------------------------------------------------------
data ChatRoom = ChatRoom {
      _timeoutManager :: !TimeoutManager
    , _userMap        :: !(MVar (HashTable UserName User))
    , _chatChannel    :: !(TChan Message)
    , _userTimeout    :: !Int               -- ^ how long users can remain
                                            -- inactive
}
