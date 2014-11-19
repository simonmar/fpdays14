{-# LANGUAGE
    StandaloneDeriving, GADTs, TypeFamilies,
    FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving,
    OverloadedStrings, DeriveDataTypeable
 #-}

module BlogDataSource.Internals2 ( initDataSource ) where


import BlogDataSource.Types

import Data.Typeable
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.Map as Map
import Control.Monad
import Data.Maybe
import Data.List
import Haxl.Core

import Database.SQLite

import Control.Exception
import System.Locale


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
    doFetch db (foldr collect emptyBatches blockedFetches)
    putStrLn "Fetch end"

type Batches
  = ( [ResultVar [PostId]]              -- FetchPosts
    , [(PostId, ResultVar PostInfo)]    -- FetchPostInfo
    , [(PostId, ResultVar PostContent)] -- FetchPostContent
    , [(PostId, ResultVar Int)]         -- FetchPostViews
    )

emptyBatches :: Batches
emptyBatches = ([],[],[],[])

collect :: BlockedFetch BlogRequest -> Batches -> Batches
collect (BlockedFetch FetchPosts v) (as,bs,cs,ds) =
  (v:as,bs,cs,ds)
collect (BlockedFetch (FetchPostInfo x) v) (as,bs,cs,ds) =
  (as,(x,v):bs,cs,ds)
collect (BlockedFetch (FetchPostContent x) v) (as,bs,cs,ds) =
  (as,bs,(x,v):cs,ds)
collect (BlockedFetch (FetchPostViews x) v) (as,bs,cs,ds) =
  (as,bs,cs,(x,v):ds)


doFetch :: SQLiteHandle -> Batches -> IO ()
doFetch db (as,bs,cs,ds) = do
  sqlMultiFetch db as id
    "select postid from postinfo;"
    (\row -> do [(_,Int id)] <- Just row; return (fromIntegral id))
    id
    (\_ ids -> Just ids)

  sqlMultiFetch db bs snd
    ("select * from postinfo where postid in " ++ idList (map fst bs))
    (\row -> do
       [(_,Int id),(_,Text datestr),(_,Text topic)] <- Just row
       date <- toDate datestr
       return (fromIntegral id, PostInfo (fromIntegral id) (Date date) topic))
    Map.fromList
    (\(x,_) -> Map.lookup x)

  sqlMultiFetch db cs snd
    ("select postid,content from postcontent where postid in " ++
       idList (map fst cs))
    (\row -> do
       [(_,Int id),(_,Text content)] <- Just row
       return (fromIntegral id, content))
    Map.fromList
    (\(x,_) -> Map.lookup x)

  sqlMultiFetch db ds snd
    ("select postid,views from postviews where postid in " ++
       idList (map fst ds))
    (\row -> do
       [(_,Int id),(_,Int views)] <- Just row
       return (fromIntegral id, fromIntegral views))
    Map.fromList
    (\(x,_) -> Map.lookup x)


sqlMultiFetch
  :: SQLiteHandle
  -> [x]
  -> (x -> ResultVar a)
  -> String
  -> (Row Value -> Maybe y)
  -> ([y] -> z)
  -> (x -> z -> Maybe a)
  -> IO ()

sqlMultiFetch _  [] _ _ _ _ _ = return ()
sqlMultiFetch db requests getvar query parserow collate extract = do
  results <- sql db query
  case results of
    Left s -> failAll (BlogDBException s)
    Right [rows] -> do
      let fetched = collate (catMaybes (map parserow rows))
      forM_ requests $ \q ->
        case extract q fetched of
          Nothing -> putFailure (getvar q) (BlogDBException "missing result")
          Just r -> putSuccess (getvar q) r
    _other -> failAll (BlogDBException "invalid result")
 where
  failAll e = forM_ requests $ \q -> putFailure (getvar q) e

idList :: [PostId] -> String
idList ids = "(" ++ intercalate "," [ show id | PostId id <- ids ] ++ ")"

sql :: SQLiteHandle -> String -> IO (Either String [[Row Value]])
sql db query = do
  putStrLn query
  execStatement db query

