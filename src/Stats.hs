module Stats where

import           Vector

mean :: Fractional a => Vector a -> a
mean v = sum v / fromIntegral (length v)

var :: Floating a => Vector a -> a
var v | length v <= 1 = error "Sample is not enough"
      | otherwise     = sum ((v .- m) .^ 2) / fromIntegral (length v - 1)
    where m = mean v

std :: Floating a => Vector a -> a
std = sqrt . var
