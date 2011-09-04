module Snap.Chat.API.Handlers 
  ( apiHandlers
  ) where

------------------------------------------------------------------------------
import           Control.Monad.Reader
import           Data.Aeson
import           Snap.Types
------------------------------------------------------------------------------
import           Snap.Chat.Internal.Types
import           Snap.Chat.Internal.API.Types


------------------------------------------------------------------------------
apiHandlers :: ChatRoom -> Snap ()
apiHandlers chatRoom = flip runReaderT chatRoom $
                       route [ ("join",  apiCall joinChannel               )
                             , ("leave", authenticatingApiCall leaveChannel)
                             , ("fetch", authenticatingApiCall fetchMessage)
                             , ("write", authenticatingApiCall writeMessage)
                             ]


------------------------------------------------------------------------------
-- | Scaffold common to all api requests. Handles:
--
-- * ensuring that the request is a POST with the correct MIME type
--   (application/json)
--
-- * decoding the request body
--
-- * running the user request, catching any exceptions, etc
--
-- * if successful, encoding the response and sending it
apiCall :: (FromJSON req, ToJSON resp) =>
           (req -> ApiHandler (ApiResponse resp))
        -> ApiHandler ()
apiCall = undefined


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
authenticatingApiCall :: (FromJSON req, ToJSON resp) =>
                         (User -> req -> ApiHandler resp)
                      -> ApiHandler ()
authenticatingApiCall = apiCall . authenticate


------------------------------------------------------------------------------
authenticate :: (FromJSON req, ToJSON resp) =>
                (User -> req -> ApiHandler resp)
             -> (ApiRequest req -> ApiHandler (ApiResponse resp))
authenticate = undefined


------------------------------------------------------------------------------
joinChannel :: JoinRequest
            -> ApiHandler (ApiResponse JoinResponse)
joinChannel = undefined


------------------------------------------------------------------------------
leaveChannel :: User -> LeaveRequest -> ApiHandler LeaveResponse
leaveChannel = undefined


------------------------------------------------------------------------------
fetchMessage :: User -> GetMessagesRequest -> ApiHandler GetMessagesResponse
fetchMessage = undefined


------------------------------------------------------------------------------
writeMessage :: User -> WriteMessageRequest -> ApiHandler WriteMessageResponse
writeMessage = undefined
