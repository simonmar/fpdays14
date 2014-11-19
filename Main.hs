module Main where

import BlogDataSource.Internals2 ( initDataSource )
import Blog           ( blog )
import Haxl.Core

main :: IO ()
main = do
  blogstate <- initDataSource
  let state = stateSet blogstate stateEmpty
  env0 <- initEnv state ()
  dat <- runHaxl env0 (do blog; dumpCacheAsHaskell)
  putStrLn dat
