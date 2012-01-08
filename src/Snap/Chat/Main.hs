{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Snap.Chat.API.Handlers
import           Snap.Chat.ChatRoom
import           Snap.Chat.Types
import           Snap.Core
import           Snap.Http.Server
import           Snap.Util.FileServe
import           Web.ClientSession

handler :: Key -> ChatRoom -> Snap ()
handler key chatRoom = route [ (""      , root                    )
                             , ("api"   , apiHandlers key chatRoom)
                             ]
  where
    root = serveDirectory "static"


main :: IO ()
main = do
    key <- getDefaultKey
    withChatRoom 200 $ quickHttpServe . handler key
