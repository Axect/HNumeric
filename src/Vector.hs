module Vector where

import Data.Functor ()
import Control.Applicative ()

newtype Vector a = Vector [a] deriving (Show, Eq)

instance Functor Vector where
    fmap f (Vector x) = Vector (map f x)

instance Applicative Vector where
    pure a = Vector [] 
    Vector fs <*> Vector xs = Vector (zipWith ($) fs xs)

instance (Num a) => Num (Vector a) where
    negate v = negate <$> v
    (+) v1 v2 = (+) <$> v1 <*> v2
    (*) v1 v2 = (*) <$> v1 <*> v2
    fromInteger n = fromInteger <$> Vector (replicate (fromIntegral n) 0)
    signum v = signum <$> v 
    abs v = abs <$> v

instance Foldable Vector where
    foldr _ z (Vector []) = z
    foldr f z (Vector xs) = foldr f z xs

    foldl _ z (Vector []) = z
    foldl f z (Vector xs) = foldl f z xs

mean :: Fractional a => Vector a -> a
mean v = sum v / fromIntegral (length v)