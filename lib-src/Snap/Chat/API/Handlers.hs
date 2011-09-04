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
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import           Prelude hiding (catch)
import           Snap.Types
import           Web.ClientSession
------------------------------------------------------------------------------
import           Snap.Chat.ChatRoom
import           Snap.Chat.Internal.Types
import           Snap.Chat.Internal.API.Types


------------------------------------------------------------------------------
apiHandlers :: Key -> ChatRoom -> Snap ()
apiHandlers key chatRoom =
    flip runReaderT chatRoom $
         route [ ("join",  apiCall handleJoin               )
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
    -- Check that the content-type is JSON.
    ct <- liftM (getHeader "Content-Type") getRequest
    when (ct /= Just "application/json") $
         finishWith $ setResponseCode 415 emptyResponse

    -- Grab the JSON request body
    jsonInput <- liftM (S.concat . L.toChunks) getRequestBody

    let parseResult = parseOnly json jsonInput
    either errorOut
           (\obj -> do
                let input = fromJSON obj
                case input of
                  Error e   -> errorOut e
                  Success a -> do
                    output <- f a
                    modifyResponse $ setContentType "application/json"
                    writeBuilder $ fromValue $ toJSON output)
           parseResult
  where
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

    auth (EncodedSession token userName) = do
        chatRoom <- ask :: ApiHandler ChatRoom
        eUser    <- try $ liftIO $ authenticateUser userName token chatRoom
        either (\(_::SomeException) -> return authenticationFailure)
               (\user -> do
                    resp <- f user requestData
                    let newSession = EncodedSession (_userToken user) userName
                    let newEncodedSession = S.concat $ L.toChunks $
                                            encode newSession
                    if isFailure resp
                      then return $ ApiResponseFailure (failureCode resp)
                                                       (failureReason resp)
                      else return $ ApiResponseSuccess newEncodedSession resp)
               eUser


------------------------------------------------------------------------------
handleJoin :: JoinRequest
            -> ApiHandler (ApiResponse JoinResponse)
handleJoin = undefined


------------------------------------------------------------------------------
handleLeave :: User -> LeaveRequest -> ApiHandler LeaveResponse
handleLeave = undefined


------------------------------------------------------------------------------
handleFetch :: User -> GetMessagesRequest -> ApiHandler GetMessagesResponse
handleFetch = undefined


------------------------------------------------------------------------------
handleWrite :: User -> WriteMessageRequest -> ApiHandler WriteMessageResponse
handleWrite = undefined
