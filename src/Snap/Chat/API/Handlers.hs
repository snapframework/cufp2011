{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Chat.API.Handlers
  ( apiHandlers
  ) where

------------------------------------------------------------------------------
import           Control.Exception (SomeException)
import           Control.Monad
import           Control.Monad.CatchIO
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Encode
import           Data.Attoparsec hiding (try)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Char
import qualified Data.Text as T
import           Prelude hiding (catch)
import           Snap.Iteratee (($$), consume, joinI, takeNoMoreThan)
import           Snap.Types
import           System.PosixCompat.Time
import           Web.ClientSession
------------------------------------------------------------------------------
import           Snap.Chat.ChatRoom
import           Snap.Chat.Internal.Types
import           Snap.Chat.Internal.API.Types


------------------------------------------------------------------------------
apiHandlers :: Key -> ChatRoom -> Snap ()
apiHandlers key chatRoom =
    flip runReaderT chatRoom $
         route [ ("join",  apiCall $ handleJoin key             )
               , ("leave", authenticatingApiCall key handleLeave)
               , ("fetch", authenticatingApiCall key handleFetch)
               , ("write", authenticatingApiCall key handleWrite)
               ]


------------------------------------------------------------------------------
-- | Scaffold common to all api requests. Handles:
--
-- * ensuring that the request is a POST with the correct MIME type
--   (application/json)
--
-- * decoding the request body
--
-- * running the user request
--
-- * if successful, encoding the response and sending it
apiCall :: (FromJSON req, ToJSON resp) =>
           (req -> ApiHandler (ApiResponse resp))
        -> ApiHandler ()
apiCall f = method POST $ do
    -- Check that the content-type is JSON. Strip off any charset suffixes.
    ct <- liftM (fmap (S.takeWhile (\c -> c /= ';' && not (isSpace c)))
                      . getHeader "Content-Type") getRequest

    when (ct /= Just "application/json") $
         finishWith $ setResponseCode 415 emptyResponse


    -- Your code goes here.
    toBeImplemented

  where
    maxSize = 131072    -- 128 kB should be enough for anybody

    fetchRequestBody :: ApiHandler ByteString
    fetchRequestBody = liftM S.concat $ runRequestBody $
                       joinI $ takeNoMoreThan maxSize $$ consume


------------------------------------------------------------------------------
-- | Scaffold common to api requests that require an authenticated user.
-- Handles:
--
-- * Decoding the input session token
--
-- * Looking up that the user is connected
--
-- * Making sure the authentication token matches
--
-- * Running the user request
--
-- * Encoding the output session token
authenticatingApiCall :: (HasStatus resp, FromJSON req, ToJSON resp) =>
                         Key
                      -> (User -> req -> ApiHandler resp)
                      -> ApiHandler ()
authenticatingApiCall key = apiCall . authenticate key


------------------------------------------------------------------------------
authenticate :: (FromJSON req, ToJSON resp, HasStatus resp) =>
                Key
             -> (User -> req -> ApiHandler resp)
             -> ApiRequest req
             -> ApiHandler (ApiResponse resp)
authenticate key f apiRequest = do
    -- Your code goes here.
    toBeImplemented


------------------------------------------------------------------------------
encodeSession :: Key -> User -> IO ByteString
encodeSession key (User name _ token _) = epochTime >>= newEncodedSession
  where
    newEncodedSession now = do
        let newSession = EncodedSession token now name
        encryptIO key $ S.concat $ L.toChunks $ encode newSession


------------------------------------------------------------------------------
handleJoin :: Key
           -> JoinRequest
           -> ApiHandler (ApiResponse JoinResponse)
handleJoin key (JoinRequest userName) = do
    -- Your code goes here.
    toBeImplemented

------------------------------------------------------------------------------
handleLeave :: User -> LeaveRequest -> ApiHandler LeaveResponse
handleLeave user _ = do
    -- Your code goes here
    toBeImplemented


------------------------------------------------------------------------------
handleFetch :: User -> GetMessagesRequest -> ApiHandler GetMessagesResponse
handleFetch user _ = do
    setTimeout $ defaultTimeout + 10
    -- Your code goes here.
    toBeImplemented


------------------------------------------------------------------------------
handleWrite :: User -> WriteMessageRequest -> ApiHandler WriteMessageResponse
handleWrite user (WriteMessageRequest msg) = do
    -- Your code goes here.
    toBeImplemented


------------------------------------------------------------------------------
defaultTimeout :: Int
defaultTimeout = 50
