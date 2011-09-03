{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Snap.Chat.Types
  ( UserName

  , Message
  , MessageContents(..)
  , getMessageUserName
  , getMessageTime
  , getMessageContents

  , UserToken
  , User
  , getUserName

  , ChatRoom
  ) where

import Snap.Chat.Internal.Types
