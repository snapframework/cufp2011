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
import           Data.Attoparsec hiding (try)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Char
import qualified Data.Text as T
import           Prelude hiding (catch)
import           Snap.Iteratee (($$), consume, joinI, takeNoMoreThan)
import           Snap.Core
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

    -- Grab the JSON request body
    jsonInput <- fetchRequestBody

    let parseResult = parseOnly json jsonInput
    either errorOut
           (\obj -> do
                let input = fromJSON obj
                case input of
                  Error e   -> errorOut e
                  Success a -> do
                    output <- f a
                    modifyResponse $ setContentType "application/json"
                    writeLBS $ encode $ toJSON output)
           parseResult
  where
    maxSize = 131072    -- 128 kB should be enough for anybody

    fetchRequestBody = liftM S.concat $ runRequestBody $
                       joinI $ takeNoMoreThan maxSize $$ consume

    errorOut e = do
        putResponse emptyResponse
        writeText $ "Error decoding JSON input:\n"
        writeText $ T.pack $ show e
        getResponse >>= finishWith . setResponseCode 415


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
    maybe (return authenticationFailure)
          (\txt -> either (const $ return authenticationFailure)
                          (\obj -> do
                               let input = fromJSON obj
                               case input of
                                 Error _      -> return authenticationFailure
                                 Success sess -> auth sess)
                          (parseOnly json txt))
          mbDecryptedText

  where
    encodedSession  = _encodedSession apiRequest
    requestData     = _requestData    apiRequest
    mbDecryptedText = decrypt key encodedSession

    auth (EncodedSession token oldTime userName) = do
        chatRoom <- ask :: ApiHandler ChatRoom
        now <- liftIO epochTime
        if now - oldTime > toEnum (_userTimeout chatRoom)
          then return authenticationFailure
          else do
            eUser    <- try $ liftIO $ authenticateUser userName token
                                                        chatRoom
            either (\(_::SomeException) -> return authenticationFailure)
                   (\user -> do
                        resp <- f user requestData
                        newEncodedSession <- liftIO $ encodeSession key user
                        if isFailure resp
                          then return $
                               ApiResponseFailure (failureCode resp)
                                                  (failureReason resp)
                          else return $
                               ApiResponseSuccess newEncodedSession resp)
                   eUser


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
    (ask >>= joinUp) `catch` \(_ :: UserAlreadyConnectedException) -> do
        return $ ApiResponseFailure (failureCode resp) (failureReason resp)
  where
    resp = JoinResponseUserAlreadyExists

    joinUp chatRoom = do
        user <- liftIO $ joinUser userName chatRoom
        newEncodedSession <- liftIO $ encodeSession key user
        return $ ApiResponseSuccess newEncodedSession JoinResponseOK


------------------------------------------------------------------------------
handleLeave :: User -> LeaveRequest -> ApiHandler LeaveResponse
handleLeave user _ = do
    ask >>= liftIO . disconnectUser userName disconnectionReason
    return LeaveResponseOK
  where
    userName = _userName user
    disconnectionReason = T.concat [ " has left the channel." ]


------------------------------------------------------------------------------
handleFetch :: User -> GetMessagesRequest -> ApiHandler GetMessagesResponse
handleFetch user _ = do
    setTimeout $ defaultTimeout + 10
    msgs <- ask >>= liftIO . getMessages defaultTimeout user
    return $ GetMessagesOK msgs


------------------------------------------------------------------------------
handleWrite :: User -> WriteMessageRequest -> ApiHandler WriteMessageResponse
handleWrite user (WriteMessageRequest msg) = do
    ask >>= liftIO . writeMessageContents msg user
    return WriteMessageResponseOK


------------------------------------------------------------------------------
defaultTimeout :: Int
defaultTimeout = 50
