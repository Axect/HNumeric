module HNum.F where

import           HNum.Vector
import           HNum.CSV

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

instance FuncTools Vector where
  hflat f = f . toList
  hlift f = vec . f . toList
  hmap = hlift . map
  hfilter = hlift . filter
  htake n = hlift (take n)
  htakeWhile f = hlift (takeWhile f)
  hdrop n = hlift (drop n)
  hdropWhile f = hlift (dropWhile f)

instance FuncTools Matrix where
  hflat = undefined
  hlift f = matrix . map f . matForm
  hmap = hlift . map
  hfilter = hlift . filter
  htake n = hlift (take n)
  htakeWhile f = hlift (takeWhile f)
  hdrop n = hlift (drop n)
  hdropWhile f = hlift (dropWhile f)

