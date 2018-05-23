module Stats where

import Vector

mean :: Fractional a => Vector a -> a
mean v = sum v / fromIntegral (length v)