module Main where

import BlogDataSource ( initDataSource )
import Blog           ( blog )
import Haxl.Core

main :: IO ()
main = do
  blogstate <- initDataSource
  let state = stateSet blogstate stateEmpty
  env0 <- initEnv state ()
  r <- runHaxl env0 blog
  print r
