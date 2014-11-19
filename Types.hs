{-# LANGUAGE
    GADTs, GeneralizedNewtypeDeriving, DeriveDataTypeable
 #-}

module Types ( Haxl ) where

import Haxl.Core

type Haxl a = GenHaxl () a

