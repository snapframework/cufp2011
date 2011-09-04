{-# LANGUAGE OverloadedStrings #-}

module Snap.Chat.Internal.API.Types where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.Aeson.Types as A
import           Data.ByteString.Char8 (ByteString)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           Snap.Types
------------------------------------------------------------------------------
import           Snap.Chat.Internal.Types


------------------------------------------------------------------------------
type ApiHandler a = ReaderT ChatRoom Snap a

------------------------------------------------------------------------------
data ApiRequest req = ApiRequest {
      _encodedSession :: ByteString
    , _apiUserName    :: Text
    , _requestType    :: req
    }


------------------------------------------------------------------------------
data ApiResponse resp = ApiResponseSuccess {
                          _newEncodedSession :: ByteString
                        , _responseType      :: resp
                        }
                      | ApiResponseFailure {
                          _failureCode   :: Text
                        , _failureReason :: Text
                        }


------------------------------------------------------------------------------
authenticationFailure :: ApiResponse a
authenticationFailure =
    ApiResponseFailure "authentication_failure" "Authentication failure."


------------------------------------------------------------------------------
class HasStatus a where
    isFailure     :: a -> Bool
    isFailure _ = False

    failureCode   :: a -> Text
    failureCode _ = "ok"

    failureReason :: a -> Text
    failureReason _ = "ok"


------------------------------------------------------------------------------
instance (FromJSON req) => FromJSON (ApiRequest req) where
    parseJSON (Object obj) = ApiRequest          <$>
                             obj .: "session"    <*>
                             obj .: "user"       <*>
                             obj .: "requestData"

    parseJSON _ = fail "ApiRequest: JSON object of wrong type"


------------------------------------------------------------------------------
instance (ToJSON resp) => ToJSON (ApiResponse resp) where
    toJSON (ApiResponseSuccess s r) =
        Object $ Map.fromList [ ("status"  , toJSON ("ok"::Text))
                              , ("session" , toJSON s           )
                              , ("response", toJSON r           )
                              ]

    toJSON (ApiResponseFailure code reason) =
        Object $ Map.fromList [ ("status"    , toJSON ("failure"::Text))
                              , ("statusCode", toJSON code             )
                              , ("reason"    , toJSON reason           )
                              ]


------------------------------------------------------------------------------
data JoinRequest = JoinRequest { _desiredUserName :: Text }
data JoinResponse = JoinResponseOK
                  | JoinResponseUserAlreadyExists


------------------------------------------------------------------------------
instance HasStatus JoinResponse where
    isFailure JoinResponseOK                = False
    isFailure JoinResponseUserAlreadyExists = True

    failureCode JoinResponseOK = "ok"
    failureCode JoinResponseUserAlreadyExists = "user_already_exists"

    failureReason JoinResponseOK = "ok"
    failureReason JoinResponseUserAlreadyExists =
        T.concat [ "Cannot log in; a user with that name is already connected "
                 , "to the channel."
                 ]


------------------------------------------------------------------------------
instance FromJSON JoinRequest where
    parseJSON (Object obj) = JoinRequest <$> obj .: "desiredUserName"
    parseJSON _            = fail "JoinRequest: JSON object of wrong type"

instance ToJSON JoinResponse where
    toJSON _ = Object Map.empty


------------------------------------------------------------------------------
data LeaveRequest = LeaveRequest
data LeaveResponse = LeaveResponseOK
instance HasStatus LeaveResponse

instance FromJSON LeaveRequest where
    parseJSON _ = pure LeaveRequest

instance ToJSON LeaveResponse where
    toJSON _ = Object Map.empty


------------------------------------------------------------------------------
data GetMessagesRequest  = GetMessagesRequest
-- authentication failures handled on a different level here, so this command
-- cannot fail.
data GetMessagesResponse = GetMessagesOK [Message]
instance HasStatus GetMessagesResponse

instance FromJSON GetMessagesRequest where
    parseJSON _ = pure GetMessagesRequest

instance ToJSON GetMessagesResponse where
    toJSON (GetMessagesOK msgs) =
        Object $ Map.fromList [ ("messages", toJSON msgs) ]


------------------------------------------------------------------------------
data WriteMessageRequest  = WriteMessageRequest Message
data WriteMessageResponse = WriteMessageResponseOK
instance HasStatus WriteMessageResponse

instance FromJSON WriteMessageRequest where
    parseJSON obj = WriteMessageRequest <$> parseJSON obj

instance ToJSON WriteMessageResponse where
    toJSON _ = Object Map.empty
