{-|
Module      : Vector
Description : Haskell Vector & Matrix & Linear Algebra Library to do machine learning
CopyRight   : (c) Tae Geun Kim, 2018
License     : GPL-3
Maintainer  : edeftg@gmail.com
Stability   : Experimental
-}
module Vector where

import           Data.Functor                   ( )
import           Control.Applicative            ( )

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

-- |toList makes List from Vector
toList :: Vector a -> [a]
toList (Vector xs) = xs

-- |fromList is equivalent to Vector constructor
fromList :: [a] -> Vector a
fromList = Vector

-- Operation
{-|
   (.+) is addition Vector with Constant.
   Dot means position of Vector.
   Example: a .* 2  = twice whole elements of a
            a .*. b = Dot product
-}
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

-- |(.^) is power function. Don't confuse with bitwise function.
(.^) :: Floating a => Vector a -> a -> Vector a
v .^ n = (** n) <$> v

-- |(.*.) is dot product of two vectors.
(.*.) :: Num a => Vector a -> Vector a -> a
v .*. w = sum $ v * w

{-|
   (%+) is addition Matrix with Constant.
   % means position of Matrix.
   Example: a %* 2  = twice whole elements of a.
            a %*% b = Matrix multiplication.
-}
(%+) :: Num a => Matrix a -> a -> Matrix a
m %+ a = map (+ a) <$> m

-- Subtraction Matrix with Constant
(%-) :: Num a => Matrix a -> a -> Matrix a
m %- a = map (+ (-a)) <$> m

-- Multiplication Matrix with Constant
(%*) :: Num a => Matrix a -> a -> Matrix a
m %* a = map (* a) <$> m

-- Divide Matrix with Constant
(%/) :: Fractional a => Matrix a -> a -> Matrix a
m %/ a = map (/ a) <$> m

-- |(%+%) is addition between two matrix.
(%+%) :: Num a => Matrix a -> Matrix a -> Matrix a
Vector [] %+% m         = m
m         %+% Vector [] = m
m         %+% n         = zipWith (+) <$> m <*> n

-- Subtraction Matrix
(%-%) :: Num a => Matrix a -> Matrix a -> Matrix a
m %-% n = zipWith (-) <$> m <*> n

-- |Matrix Multiplication using Devide and Conquer Algorithm.
(%*%) :: Num a => Matrix a -> Matrix a -> Matrix a
_            %*% Vector []    = Vector []
Vector []    %*% _            = Vector []
_            %*% Vector [[]]  = Vector [[]]
Vector [[] ] %*% _            = Vector [[]]
Vector [[x]] %*% Vector [[y]] = Vector [[x * y]]
m            %*% n            = (a11 %++% a12) %**% (a21 %++% a22)
 where
  (m11, n11) = (bp 1 m, bp 1 n)
  (m12, n12) = (bp 2 m, bp 2 n)
  (m21, n21) = (bp 3 m, bp 3 n)
  (m22, n22) = (bp 4 m, bp 4 n)
  a11        = (m11 %*% n11) %+% (m12 %*% n21)
  a12        = (m11 %*% n12) %+% (m12 %*% n22)
  a21        = (m21 %*% n11) %+% (m22 %*% n21)
  a22        = (m21 %*% n12) %+% (m22 %*% n22)

-- |Block Partitioning
bp :: Int -> Matrix a -> Matrix a
bp n m = fromList $ bpMat n (toList m)

-- TODO: Multiplicate Inverse Matrix
--(%/%) :: Fractional a => Matrix a -> Matrix a -> Matrix a
--m %/% n = zipWith (/) <$> m <*> n

-- Concatenate
-- |(.++.) horizontally concatenate vectors.
(.++.) :: Vector a -> Vector a -> Vector a
v .++. w = fromList (toList v ++ toList w)

-- |hcat is same as (.++.). It is just julia syntax.
hcat :: Vector a -> Vector a -> Vector a
hcat v w = v .++. w

-- |(.**.) vertically stack vectors.
(.**.) :: Vector a -> Vector a -> Matrix a
v .**. w = fromList (toList v : [toList w])

-- |vcat is same as (.**.)
vcat :: Vector a -> Vector a -> Matrix a
vcat v w = v .**. w

-- |(.:) inserts vector to head of matrix.
(.:) :: Vector a -> Matrix a -> Matrix a
v .: m = fromList (toList v : toList m)

-- |(%++%) Horizontally concatenate matrices.
(%++%) :: Matrix a -> Matrix a -> Matrix a
m %++% n = fromList $ zipWith (++) (toList m) (toList n)

-- |(%**%) Vertically concatenate matrices.
(%**%) :: Matrix a -> Matrix a -> Matrix a
m %**% n = fromList (toList m ++ toList n)

-- |Norm is norm of vector.
norm :: Floating a => Vector a -> a
norm v = sqrt $ v .*. v

-- Matrix Implementation
-- |transpose is transpose of matrix.
transpose :: Matrix a -> Matrix a
transpose = fromList . transposeMat . toList

-- |index represents index of matrix of that position.
-- Example: index [[1,2],[3,4]] = [[(0,0), (0,1)], [(1,0),(1,1)]]
index :: Matrix a -> [[(Int, Int)]]
index = indexMat . toList

-- |det is determinant of matrix.
det :: Num a => Matrix a -> a
det m | isSquare m = (detMat . toList) m
      | otherwise  = error "It's not Square matrix"

-- |isSquare
isSquare :: Matrix a -> Bool
isSquare m = all (== length m) (length <$> m)

-- |isInvertible
isInvertible :: (Eq a, Num a) => Matrix a -> Bool
isInvertible m = det m /= 0

-- |inv is inverse of matrix.
inv :: (Eq a, Fractional a) => Matrix a -> Matrix a
inv m | isInvertible m = (fromList . invMat . toList) m
      | otherwise      = error "Matrix is not invertible!"

-- Useful Function
-- Transpose
transposeMat :: [[a]] -> [[a]]
transposeMat m = map (\l -> map (!! l) m) [0 .. (length (head m) - 1)]

-- indexMat
indexMat :: [[a]] -> [[(Int, Int)]]
indexMat m@(xs : xss) = do
  i <- [0 .. (length m - 1)]
  [zip (replicate (length xs) i) [0 .. (length xs - 1)]]

-- dropAtMat
dropAtMat :: Int -> Int -> [[a]] -> [[a]]
dropAtMat i j mat = map (dropAt j) $ dropAt i mat

-- postSplitAt
postSplitAt (x, y) = x ++ tail y

-- dropAt
dropAt :: Int -> [a] -> [a]
dropAt i = postSplitAt . splitAt i

-- dropAtMat' : drop nth array
dropAtMat' :: Int -> [[a]] -> [[a]]
dropAtMat' n mat | n /= (length mat - 1) = dropAt n mat
                 | otherwise             = take n mat

-- minorMat
minorMat :: Int -> [[a]] -> [[a]]
minorMat i m = map tail (dropAtMat' i m)

-- picewise Determinant
pwDet :: Num a => Int -> [[a]] -> a
pwDet _ [[a]] = a
pwDet n mat   = (-1) ^ n * head (mat !! n) * sum
  [ pwDet m mat2 | m <- [0 .. (length mat2 - 1)] ]
  where mat2 = minorMat n mat

-- det for [[a]]
detMat :: Num a => [[a]] -> a
detMat mat = sum [ pwDet n mat | n <- [0 .. (length mat - 1)] ]

-- cofactor
cofactorMat :: Num a => Int -> Int -> [[a]] -> a
cofactorMat i j = (* (-1) ^ (i + j)) . detMat . dropAtMat i j

--inverse
invMat :: Fractional a => [[a]] -> [[a]]
invMat = toMat . invFlat

invFlat :: Fractional a => [[a]] -> [a]
invFlat m = do
  let d   = abs (detMat m)
  let idx = (transposeMat . indexMat) m
  (i, j) <- concat idx
  [cofactorMat i j m / d]

-- Vector to Mat
toMat :: [a] -> [[a]]
toMat m = do
  i <- [0 .. (l - 1)]
  [take l (drop (l * i) m)]
  where l = (floor . sqrt) (fromIntegral (length m))

-- Block Partitioning
bpMat :: Int -> [[a]] -> [[a]]
bpMat _ [] = []
bpMat n m | n == 1    = (map (take sl) . take sl) m
          | n == 2    = (map (drop sl) . take sl) m
          | n == 3    = (map (take sl) . drop sl) m
          | n == 4    = (map (drop sl) . drop sl) m
          | otherwise = error "Please input 1 ~ 4"
 where
  l  = length m
  sl = (floor . sqrt . fromIntegral) l
