module Vector where

import           Data.Functor                             ( )
import           Control.Applicative                      ( )

-- Vector Implementation
newtype Vector a = Vector [a] deriving (Show, Eq)
type Matrix a = Vector [a]

instance Functor Vector where
    fmap f (Vector x) = Vector (map f x)

instance Applicative Vector where
    pure a = Vector []
    Vector fs <*> Vector xs = Vector (zipWith ($) fs xs)

instance (Num a) => Num (Vector a) where
    negate v       = negate <$> v
    (+) v1 v2      = (+) <$> v1 <*> v2
    (*) v1 v2      = (*) <$> v1 <*> v2
    fromInteger n  = fromInteger <$> Vector (replicate (fromIntegral n) 0)
    signum v       = signum <$> v
    abs v          = abs <$> v

instance Foldable Vector where
    foldr _ z (Vector []) = z
    foldr f z (Vector xs) = foldr f z xs

    foldl _ z (Vector []) = z
    foldl f z (Vector xs) = foldl f z xs

-- Addition
(.+) :: Num a => Vector a -> a -> Vector a
v .+ n = (+ n) <$> v

-- Subtraction
(.-) :: Num a => Vector a -> a -> Vector a
v .- n = (+ (-n)) <$> v

-- Multiplication
(.*) :: Num a => Vector a -> a -> Vector a
v .* n = (* n) <$> v

-- Divide
(./) :: Fractional a => Vector a -> a -> Vector a
v ./ n = (/ n) <$> v

-- Power (matlab syntax)
(.^) :: Floating a => Vector a -> a -> Vector a
v .^ n = (** n) <$> v

-- Dot product
(.*.) :: Num a => Vector a -> Vector a -> a
v .*. w = sum $ v * w

-- Norm
norm :: Floating a => Vector a -> a
norm v = sqrt $ v .*. v

-- Matrix Implementation

-- determinant
