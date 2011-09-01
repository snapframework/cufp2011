module Snap.Chat.Internal.Util 
  ( seconds
  ) where


------------------------------------------------------------------------------
seconds :: Int -> Int
seconds n = n * ((10::Int)^(6::Int))

