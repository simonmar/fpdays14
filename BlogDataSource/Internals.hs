{-# LANGUAGE
    TypeFamilies, MultiParamTypeClasses, FlexibleInstances,
    OverloadedStrings
 #-}

module BlogDataSource.Internals (
    initDataSource,
    BlogRequest(..)
  ) where

import BlogDataSource.Types

import Haxl.Core
import Database.SQLite

-- -----------------------------------------------------------------------------
-- Data source internals

instance StateKey BlogRequest where
  data State BlogRequest = BlogDataState SQLiteHandle

initDataSource :: IO (State BlogRequest)
initDataSource = do
  db <- openConnection "blog.sqlite"
  return $ BlogDataState db

instance DataSourceName BlogRequest where
  dataSourceName _ = "BlogDataSource"

instance DataSource u BlogRequest where
  fetch (BlogDataState db) _flags _userEnv blockedFetches = SyncFetch $ do
    putStrLn "Fetch start"
    mapM_ (doFetch db) blockedFetches
    putStrLn "Fetch end"


doFetch :: SQLiteHandle -> BlockedFetch BlogRequest -> IO ()
doFetch db (BlockedFetch FetchPosts v) = do
  r <- sql db "select postid from postinfo;"
  case r of
    Left s -> putFailure v (BlogDBException s)
    Right [rows] -> putSuccess v [ fromIntegral id | [(_,Int id)] <- rows ]
    _ -> putFailure v (BlogDBException "invalid result")

doFetch db (BlockedFetch (FetchPostInfo (PostId x)) v) = do
  r <- sql db ("select * from postinfo where postid = " ++ show x ++ ";")
  case r of
    Left s -> putFailure v (BlogDBException s)
    Right [[[(_,Int id),(_,Text datestr),(_,Text topic)]]]
      | Just date <- toDate datestr ->
      putSuccess v (PostInfo (fromIntegral id) (Date date) topic)
    _ -> putFailure v (BlogDBException "invalid result")

doFetch db (BlockedFetch (FetchPostContent (PostId x)) v) = do
  r <- sql db ("select content from postcontent where postid = " ++
            show x ++ ";")
  case r of
    Right [[[(_,Text str)]]] -> putSuccess v str
    Left s -> putFailure v (BlogDBException s)
    _ -> putFailure v (BlogDBException "invalid result")

doFetch db (BlockedFetch (FetchPostViews (PostId x)) v) = do
  r <- sql db ("select views from postviews where postid = " ++ show x ++ ";")
  case r of
    Right [[[(_,Int n)]]] -> putSuccess v (fromIntegral n)
    Left s -> putFailure v (BlogDBException s)
    _ -> putFailure v (BlogDBException "invalid result")

sql :: SQLiteHandle -> String -> IO (Either String [[Row Value]])
sql db query = do
  putStrLn query
  execStatement db query
