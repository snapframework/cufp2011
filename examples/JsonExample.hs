{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Data.Aeson
import           Data.Attoparsec (parseOnly)
import           Data.ByteString.Char8  (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L


------------------------------------------------------------------------------
example1 :: ByteString -> Either String Coord
example1 bs = parseOnly json bs >>= convert
  where
    convert value = case fromJSON value of
                      (Error e)   -> Left e
                      (Success a) -> Right a

example2 :: Coord -> ByteString
example2 c = S.concat $ L.toChunks $ encode c


------------------------------------------------------------------------------
data Coord = Coord { _x :: Double, _y :: Double }
  deriving (Show, Eq)

instance ToJSON Coord where
    toJSON (Coord x y) = object ["x" .= x, "y" .= y]


instance FromJSON Coord where
    parseJSON (Object v) = Coord    <$>
                           v .: "x" <*>
                           v .: "y"

    -- A non-Object value is of the wrong type, so use mzero to fail.
    parseJSON _          = empty
