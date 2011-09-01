module Snap.Chat.Message
  ( talk
  , action
  , join
  , leave
  , messageTime
  , messageUser
  , messageContents
  ) where

------------------------------------------------------------------------------
import           Data.Text (Text)
import           System.PosixCompat.Time
import           System.Posix.Types
------------------------------------------------------------------------------
import           Snap.Chat.Internal.Types


------------------------------------------------------------------------------
talk :: Text -> User -> IO Message
talk = newMessage . Talk


------------------------------------------------------------------------------
action :: Text -> User -> IO Message
action = newMessage . Action


------------------------------------------------------------------------------
leave :: Text -> User -> IO Message
leave = newMessage . Leave


------------------------------------------------------------------------------
join :: User -> IO Message
join = newMessage Join


------------------------------------------------------------------------------
newMessage :: MessageContents -> User -> IO Message
newMessage c user = do
    now <- epochTime
    return $! Message userName now c
  where
    userName = _userName user


------------------------------------------------------------------------------
messageTime :: Message -> EpochTime
messageTime = _messageTime


------------------------------------------------------------------------------
messageUser :: Message -> UserName
messageUser = _messageUser


------------------------------------------------------------------------------
messageContents :: Message -> MessageContents
messageContents = _messageContents
