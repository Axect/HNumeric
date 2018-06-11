{-
Module      : HNumeric.Stats
Description : Haskell Statistics Library with HNum.Vector
CopyRight   : (c) Tae Geun Kim, 2018
License     : BSD3
Maintainer  : edeftg@gmail.com
Stability   : Experimental
-}
module HNum.Stats where

import           HNum.Vector
import           Data.Random.Normal
import           System.Random
import           HNum.CSV

-- | To contain coefficients of linear regression.
type Coeff a = (a, a)
--------------------------------------------------------
-- Basic Probability
--------------------------------------------------------
-- | Factorial
fac :: Integral a => a -> a
fac 0 = 1
fac 1 = 1
fac n = product [1 .. n]

-- | Factorial with start n,end s
facStop :: Integral a => a -> a -> a
facStop n s = product [s .. n]

-- | Permutation
p :: Integral a => a -> a -> a
n `p` r = facStop n (n - r + 1)

-- | Combination using permutation
c :: Integral a => a -> a -> a
n `c` r = (n `p` r) `div` fac r

--------------------------------------------------------
-- Basic Statistics
--------------------------------------------------------
-- | Basic Statistics Class for Vector
class VecOps v => Statistical v where
  -- | Sample Mean
  mean :: Fractional a => v a -> a
  -- | Single Valued covariance
  cov' :: Floating a => v a -> v a -> a
  -- | Covariance Matrix
  cov :: Floating a => v a -> v a -> Matrix a
  -- | Sample Variance
  var :: Floating a => v a -> a
  -- | Sample Standard deviation
  std :: Floating a => v a -> a
  -- | Standard Error
  se :: Floating a => v a -> a
  -- | Correlation Coefficient
  cor :: Floating a => v a -> v a -> a
  -- | Median
  med :: (Ord a, Floating a) => v a -> a
  -- | Mode
  mode :: Eq a => v a -> a
  -- | Coefficient of Variation
  cv :: Floating a => v a -> a
  -- | Moment
  moment :: Floating a => a -> v a-> a
  -- | Skewness
  skew :: Floating a => v a -> a
  -- | Skewness 2
  skew' :: Floating a => v a -> a
  -- | kurtosis
  kurt :: Floating a => v a -> a

instance Statistical Vector where
  -- mean
  mean x = sum x / fromIntegral (length x)
  -- cov'
  cov' x y
    | length x <= 1 || length y <= 1 = error "Samples are not enough"
    | length x /= length y = error "Length is not same"
    | otherwise = ((x .- mean x) .*. (y .- mean y)) / fromIntegral (length x - 1)
  -- cov
  cov x y = matrix [[var x, cov' x y], [cov' y x, var y]]
  -- var
  var v = cov' v v
  -- std
  std = sqrt . var
  -- se
  se x = std x / sqrt (fromIntegral (length x))
  -- cor
  cor x y = cov' x y / (std x * std y)
  -- med
  med x | even l    = ((qs !! (l'-1)) + (qs !! l')) / 2
        | otherwise = qs !! l'
    where l  = length x
          l' = l `div` 2
          qs = (toList . qsort) x
  -- mode
  mode x = v !! n
    where v  = toList x
          cx = map (`count` v) v
          m  = maximum cx
          n  = head $ dropWhile (\p -> cx !! p /= m) [0..]
  -- cv
  cv x = std x / mean x
  -- moment
  moment n x = sum ((x .- mean x) .^ n)
  -- skew
  skew x = (1 / fromIntegral l) * moment 3 x / std x ^ 3
    where l = length x
  skew' x = (fromIntegral l^2 / fromIntegral ((l-1) * (l-2))) * skew x
    where l = length x
  -- kurt
  kurt x = moment 4 x / (fromIntegral l * std x ** 4) - 3
    where l = length x

--------------------------------------------------------
-- For IO
--------------------------------------------------------
summary :: (Show a, Floating a) => DataFrame a -> IO ()
summary df = do
  putStrLn $ "Mean: " ++ show hm
  putStrLn $ "Var:  " ++ show hv
  putStrLn $ "Std:  " ++ show hs
 where
  h  = header df
  m  = matForm $ dat df
  ms = map (mean . vector) m
  vs = map (var . vector) m
  ss = map (std . vector) m
  hm = zip h ms
  hv = zip h vs
  hs = zip h ss

describe :: (Show a, Floating a, Ord a) => Vector a -> IO ()
describe v = do
  putStrLn $ "n:    " ++ show (length v)
  putStrLn $ "mean: " ++ show (mean v)
  putStrLn $ "std:  " ++ show (std v)
  putStrLn $ "med:  " ++ show (med v)
  putStrLn $ "mode: " ++ show (mode v)
  putStrLn $ "min:  " ++ show (minimum v)
  putStrLn $ "max:  " ++ show (maximum v)
  putStrLn $ "skew: " ++ show (skew v)
  putStrLn $ "kurt: " ++ show (kurt v)
  putStrLn $ "SE:   " ++ show (se v)
--------------------------------------------------------
-- Linear Regression
--------------------------------------------------------
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

--------------------------------------------------------
-- Backend Functions
--------------------------------------------------------

-- | Count Elements
count :: Eq a => a -> [a] -> Int
count p v = length (filter (== p) v)
