{- 
Module      : HNum.F
Description : Missing Functional Programming tools for HNumeric
CopyRight   : (c) Tae Geun Kim, 2018
License     : BSD3
Maintainer  : edeftg@gmail.com
Stability   : Experimental
-}
module HNum.F where

import           Data.Functor                   ( )

-- | Functional Programming Tools for HNum Object
class Functor f => FuncTools f where
  -- | Absolute Abstraction with Scalar Function
  hflat :: ([a] -> a) -> f a -> a
  -- | Absolute Abstraction with Vector Function
  hlift :: ([a] -> [b]) -> f a -> f b
  -- | Like map
  hmap :: (a -> b) -> f a -> f b
  -- | Like filter
  hfilter :: (a -> Bool) -> f a -> f a
  -- | Like take
  htake :: Int -> f a -> f a
  -- | Like takeWhile
  htakeWhile :: (a -> Bool) -> f a -> f a
  -- | Like drop
  hdrop :: Int -> f a -> f a
  -- | Like dropWhile
  hdropWhile :: (a -> Bool) -> f a -> f a

