{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Snap.Chat.Test.Common
  ( coverEqInstance
  , coverOrdInstance
  , coverReadInstance
  , coverShowInstance
  , coverTypeableInstance
  , forceSameType
  , expectException
  , expectExceptionH
  , eatException
  ) where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Exception (SomeException(..), evaluate)
import           Control.Monad
import           Control.Monad.CatchIO
import           Control.Monad.Trans
import           Data.Typeable
import           Prelude hiding (catch)
import qualified Data.Text as T
import           Data.Text (Text)
import           Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QC
import           Test.QuickCheck.Monadic
import           Snap.Chat.Internal.Types


------------------------------------------------------------------------------
-- | Kill the false negative on derived show instances.
coverShowInstance :: (Monad m, Show a) => a -> m ()
coverShowInstance x = a `deepseq` b `deepseq` c `deepseq` return ()
  where
    a = showsPrec 0 x ""
    b = show x
    c = showList [x] ""


eatException :: (MonadCatchIO m) => m a -> m ()
eatException a = (a >> return ()) `catch` handler
  where
    handler :: (MonadCatchIO m) => SomeException -> m ()
    handler _ = return ()


forceSameType :: a -> a -> a
forceSameType _ a = a


coverReadInstance :: (MonadIO m, Read a) => a -> m ()
coverReadInstance x = do
    liftIO $ eatException $ evaluate $ forceSameType [(x,"")] $ readsPrec 0 ""
    liftIO $ eatException $ evaluate $ forceSameType [([x],"")] $ readList ""


coverEqInstance :: (Monad m, Eq a) => a -> m ()
coverEqInstance x = a `seq` b `seq` return ()
  where
    a = x == x
    b = x /= x


coverOrdInstance :: (Monad m, Ord a) => a -> m ()
coverOrdInstance x = a `deepseq` b `deepseq` return ()
  where
    a = [ x < x
        , x >= x
        , x > x
        , x <= x 
        , compare x x == EQ ]

    b = min a $ max a a


coverTypeableInstance :: (Monad m, Typeable a) => a -> m ()
coverTypeableInstance a = typeOf a `seq` return ()


expectException :: IO a -> PropertyM IO ()
expectException m = do
    e <- QC.run $ try m
    case e of
      Left (z::SomeException)  -> (length $ show z) `seq` return ()
      Right _ -> fail "expected exception, didn't get one"


expectExceptionH :: IO a -> IO ()
expectExceptionH act = do
    e <- try act
    case e of
      Left (z::SomeException) -> (length $ show z) `seq` return ()
      Right _ -> fail "expected exception, didn't get one"



------------------------------------------------------------------------------
instance Arbitrary Text where
    arbitrary = do
        -- we don't need a full character set here
        txt <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ " ._"
        return $ T.pack txt


------------------------------------------------------------------------------
instance Arbitrary MessageContents where
    arbitrary = do
        t <- arbitrary `suchThat` (not . T.null)
        f <- elements flist
        return $ f t
      where
        flist = [ Talk
                , Action
                , \_ -> Join
                , Leave ]


------------------------------------------------------------------------------
instance Arbitrary Message where
    arbitrary = Message                               <$>
                  arbitrary `suchThat` (not . T.null) <*>
                  (toEnum <$> choose (l,h))           <*>
                  arbitrary
      where
        l = 1314070521
        h = 1344070521
