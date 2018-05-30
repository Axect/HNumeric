module Vector where

import Data.Functor                             ( )
import Control.Applicative                      ( )

-- Vector Implementation
newtype Vector a = Vector [a] deriving (Show, Eq)
type Matrix a    = Vector [a]

instance Functor Vector where
  fmap f (Vector x) = Vector (map f x)

instance Applicative Vector where
  pure a                  = Vector []
  Vector fs <*> Vector xs = Vector (zipWith ($) fs xs)

instance (Num a) => Num (Vector a) where
  negate v      = negate <$> v
  (+) v1 v2     = (+) <$> v1 <*> v2
  (*) v1 v2     = (*) <$> v1 <*> v2
  fromInteger n = fromInteger <$> Vector [n]
  signum v      = signum <$> v
  abs v         = abs <$> v

instance (Fractional a) => Fractional (Vector a) where
  recip v        = recip <$> v
  (/) v1 v2      = (*) <$> v1 <*> recip v2
  fromRational n = fromRational <$> Vector [n]

instance Foldable Vector where
  foldr _ z (Vector []) = z
  foldr f z (Vector xs) = foldr f z xs

  foldl _ z (Vector []) = z
  foldl f z (Vector xs) = foldl f z xs

-- toList
toList :: Vector a -> [a]
toList (Vector xs) = xs

-- fromList
fromList :: [a] -> Vector a
fromList = Vector

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
-- Transpose
transpose :: Matrix a -> Matrix a
transpose = fromList . transposeMat . toList

-- indexMat
index :: Matrix a -> [[(Int, Int)]]
index = indexMat . toList

-- determinant
det :: Num a => Matrix a -> a
det m | isSquare m = (detMat . toList) m
      | otherwise  = error "It's not Square matrix"

isSquare :: Matrix a -> Bool
isSquare m = all (==length m) (length <$> m)

--inv :: Num a => Matrix a -> a
--inv m | length m == 1 = (head . head . toList) m
--  | otherwise =  




-- Useful Function
-- Transpose
transposeMat :: Matrix -> Matrix
transposeMat m = map (\l -> map (!! l) m) [0 .. (length m - 1)]

-- indexMat
indexMat :: [[a]] -> [[(Int, Int)]]
indexMat m@(xs:xss) = do
  i <- [0..(length m - 1)]
  [zip (replicate (length xs) i) [0..(length xs - 1)]]

-- dropAt
dropAt :: Int -> Int -> [[a]] -> [[a]]
dropAt i j mat = map (splitAt' i) $ splitAt' j mat

-- postSplitAt
postSplitAt (x,y) = x ++ tail y

-- splitAt'
splitAt' :: Int -> [a] -> [a]
splitAt' i = postSplitAt . splitAt i

-- dropArray : drop nth array
dropArray :: Int -> [[a]] -> [[a]]
dropArray n mat | n /= (length mat - 1) = splitAt' n mat
                | otherwise             = take n mat

-- minorMat
minorMat :: Int -> [[a]] -> [[a]]
minorMat i m = map tail (dropArray i m)

-- picewise Determinant
pwDet :: Num a => Int -> [[a]] -> a
pwDet _ [[a]]   = a
pwDet n mat     = (-1) ^ n * head (mat !! n) * sum [pwDet m mat2 | m <- [0 .. (length mat2 - 1)]]
  where mat2    = minorMat n mat

-- det for [[a]]
detMat :: Num a => [[a]] -> a
detMat mat = sum [ pwDet n mat | n <- [0 .. (length mat - 1)]]

-- cofactor
cofactorMat :: Num a => Int -> Int -> [[a]] -> [[a]]
cofactorMat i j mat = map (map (* (-1)^(i+j))) (dropAt i j mat)
