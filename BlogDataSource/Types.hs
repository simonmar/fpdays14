{-# LANGUAGE
    GADTs, GeneralizedNewtypeDeriving, DeriveDataTypeable,
    StandaloneDeriving
 #-}

module BlogDataSource.Types
  ( PostId(..), Date(..), toDate, PostContent, PostInfo(..)
  , BlogDBException(..)
  , BlogRequest(..)
  ) where

import Haxl.Core

import Data.Hashable
import Data.Typeable
import Data.Time.Format
import Data.Time.Clock.POSIX
import Control.Exception
import System.Locale


-- -----------------------------------------------------------------------------
-- Basic types

-- PostId

newtype PostId = PostId Int
  deriving (Eq, Ord, Num, Hashable, Typeable, Show)


-- Dates

newtype Date = Date POSIXTime
  deriving (Eq, Ord, Typeable)

instance Show Date where
  show (Date t) = "Date " ++ show (realToFrac t :: Double)

toDate :: String -> Maybe POSIXTime
toDate = fmap utcTimeToPOSIXSeconds . parseTime defaultTimeLocale "%F %T"


-- PostContent

type PostContent = String


-- PostInfo

data PostInfo = PostInfo
  { postId    :: PostId
  , postDate  :: Date
  , postTopic :: String
  }
  deriving (Typeable, Show)


-- -----------------------------------------------------------------------------
-- Exceptions

data BlogDBException = BlogDBException String
  deriving (Show, Typeable)

instance Exception BlogDBException where
  toException = transientErrorToException
  fromException = transientErrorFromException


-- -----------------------------------------------------------------------------
-- The request type


data BlogRequest a where
  FetchPosts       :: BlogRequest [PostId]
  FetchPostInfo    :: PostId -> BlogRequest PostInfo
  FetchPostContent :: PostId -> BlogRequest PostContent
  FetchPostViews   :: PostId -> BlogRequest Int

deriving instance Show (BlogRequest a)
deriving instance Typeable BlogRequest

instance Show1 BlogRequest where show1 = show

deriving instance Eq (BlogRequest a)

instance Hashable (BlogRequest a) where
  hashWithSalt salt FetchPosts = hashWithSalt salt (0::Int)
  hashWithSalt salt (FetchPostInfo p) = hashWithSalt salt (1::Int, p)
  hashWithSalt salt (FetchPostContent p) = hashWithSalt salt (2::Int, p)
  hashWithSalt salt (FetchPostViews p) = hashWithSalt salt (3::Int, p)
