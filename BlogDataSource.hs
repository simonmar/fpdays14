{-# LANGUAGE
    StandaloneDeriving, GADTs, TypeFamilies,
    FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving,
    OverloadedStrings, DeriveDataTypeable
 #-}

module BlogDataSource (
    -- * User API
    PostId(..), Date(..), PostContent, PostInfo(..),
    getPostIds, getPostInfo, getPostContent, getPostViews,

    -- * Data source API
    initDataSource,
  ) where


import BlogDataSource.Types
import BlogDataSource.Internals

import Haxl.Core

-- -----------------------------------------------------------------------------
-- Requests

type Haxl a = GenHaxl () a

getPostIds     :: Haxl [PostId]
getPostInfo    :: PostId -> Haxl PostInfo
getPostContent :: PostId -> Haxl PostContent
getPostViews   :: PostId -> Haxl Int

getPostIds     = dataFetch FetchPosts
getPostInfo    = dataFetch . FetchPostInfo
getPostContent = dataFetch . FetchPostContent
getPostViews   = dataFetch . FetchPostViews

