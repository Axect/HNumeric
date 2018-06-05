module HNum.Stats where

import           HNum.Vector

type Coeff a = (a, a)

mean :: Fractional a => Vector a -> a
mean v = sum v / fromIntegral (length v)

cov' :: Floating a => Vector a -> Vector a -> a
cov' x y
  | length x <= 1 || length y <= 1 = error "Samples are not enough"
  | length x /= length y = error "Length is not same"
  | otherwise = ((x .- mean x) .*. (y .- mean y)) / fromIntegral (length x - 1)

var :: Floating a => Vector a -> a
var v = cov' v v

std :: Floating a => Vector a -> a
std = sqrt . var

cov :: Floating a => Vector a -> Vector a -> Matrix a
cov x y = Matrix [[var x, cov' x y], [cov' y x, var y]]

-- Least Square Method - (Intercept, Slope)
lm :: Floating a => Vector a -> Vector a -> Coeff a
lm x y = ((my - b1 * mx), b1)
 where
  mx = mean x
  my = mean y
  b1 = (x .- mx) .*. (y .- my) / ((x .- mx) .*. (x .- mx))

lineFit :: Floating a => Coeff a -> Vector a -> Vector a
lineFit (n, m) x = x .* m .+ n

rss :: Floating a => Vector a -> Vector a -> a
rss x y = sum ((y - lineFit (lm x y) x) .^ 2)

rse x y = sqrt (1 / fromIntegral (length x - 2) * rss x y)
