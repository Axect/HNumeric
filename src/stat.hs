module Stat where

main :: IO ()
main = do
  let a = [1, 2, 3] :: Vector
      b = [3, 2, 1] :: Vector
      c = [1, 2]    :: Vector
  print $ calcMat (covM (hcat a b))
  print $ calc $ cov a c

-- type alias
type Scalar = Double
type Vector = [Scalar]
type Matrix = [Vector]

hcat :: Vector -> Vector -> Matrix
hcat = zipWith (\x y -> [x, y])

mean :: Vector -> Scalar
mean v = sum v / fromIntegral (length v)

-- Define methods for Vector
(.-) :: Vector -> Scalar -> Vector
(.-) v s = map (\x -> x - s) v

(.*.) :: Vector -> Vector -> Vector
(.*.) = zipWith (*)

-- Covariance
cov :: Vector -> Vector -> Maybe Scalar
cov x y | lx /= ly = Nothing where (lx, ly) = (length x, length y)
cov x y            = Just (l / (l - 1) * mean ((x .- mean x) .*. (y .- mean y)))
  where l = fromIntegral (length x)

-- Variance
var :: Vector -> Maybe Scalar
var v = cov v v

-- Be caution!
calc :: Maybe a -> a
calc (Just x) = x
calc Nothing  = error "Nothing to calculation!"

calcMat :: [[Maybe a]] -> [[a]]
calcMat = map (map calc)

-- Monad!
covM :: Matrix -> [[Maybe Scalar]]
covM m = do
  let x = map (!! 0) m
  let y = map (!! 1) m
  i <- [x, y]
  [[cov x i, cov i y]]
