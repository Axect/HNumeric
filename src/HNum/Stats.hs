module HNum.Stats where

import           HNum.Vector

-- | To contain coefficients of linear regression.
type Coeff a = (a, a)

class VecOps v => Statistical v where
  mean :: Fractional a => v a -> a
  cov' :: Floating a => v a -> v a -> a
  cov :: Floating a => v a -> v a -> Matrix a
  var :: Floating a => v a -> a
  std :: Floating a => v a -> a
  cor :: Floating a => v a -> v a -> a

instance Statistical Vector where
  mean x = sum x / fromIntegral (length x)
  cov' x y
    | length x <= 1 || length y <= 1 = error "Samples are not enough"
    | length x /= length y = error "Length is not same"
    | otherwise = ((x .- mean x) .*. (y .- mean y)) / fromIntegral (length x - 1)
  cov x y = matrix [[var x, cov' x y], [cov' y x, var y]]
  var v = cov' v v
  std = sqrt . var
  cor x y = cov' x y / (std x * std y)

-- | Least Square Method - (Intercept, Slope)
lm :: Floating a => Vector a -> Vector a -> Coeff a
lm x y = (my - b1 * mx, b1)
 where
  mx = mean x
  my = mean y
  b1 = (x .- mx) .*. (y .- my) / ((x .- mx) .*. (x .- mx))

-- | Line Fitting with (Intercept, Slope) & Range of x
lineFit :: Floating a => Coeff a -> Vector a -> Vector a
lineFit (n, m) x = x .* m .+ n

-- | Residual Sum of Squares
rss :: Floating a => Vector a -> Vector a -> a
rss x y = sum ((y - lineFit (lm x y) x) .^ 2)

-- | Relative Standard Error
rse :: Floating a => Vector a -> Vector a -> a
rse x y = sqrt (1 / fromIntegral (length x - 2) * rss x y)
