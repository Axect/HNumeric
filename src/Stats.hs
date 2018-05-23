module Stats where

import           Vector

mean :: Fractional a => Vector a -> a
mean v = sum v / fromIntegral (length v)

cov' :: Floating a => Vector a -> Vector a -> a
cov' x y | length x <= 1 || length y <= 1 = error "Samples are not enough"
         | length x /= length y = error "Length is not same"
         | otherwise = ((x .- mean x) .*. (y .- mean y)) / fromIntegral (length x - 1)

var :: Floating a => Vector a -> a
var v = cov' v v

std :: Floating a => Vector a -> a
std = sqrt . var

cov :: Floating a => Vector a -> Vector a -> Matrix a
cov x y = Vector [[var x, cov' x y], [cov' y x, var y]]