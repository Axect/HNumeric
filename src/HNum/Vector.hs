{-|
Module      : HNumeric.Vector
Description : Haskell Vector & Matrix & Linear Algebra Library to do machine learning
CopyRight   : (c) Tae Geun Kim, 2018
License     : GPL-3
Maintainer  : edeftg@gmail.com
Stability   : Experimental
-}
module HNum.Vector where

import           Data.Functor                   ( )
import           Control.Applicative            ( )

---------------------------------------------------
-- Vector
--------------------------------------------------
-- Type Section
newtype Vector a = Vector [a] deriving (Show, Eq)

-- Instance Section
instance Functor Vector where
  fmap f (Vector x) = Vector (fmap f x)

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

-- Class for Vector with List
class List m where
  toList :: m a -> [a]
  fromList :: [a] -> m a

instance List Vector where
  toList (Vector xs) = xs
  fromList = Vector

---------------------------------------------------
-- Matrix
---------------------------------------------------

-- |Definition of Matrix
data Matrix a = Matrix {val :: Vector a, row :: Int, col :: Int, byRow :: Bool} deriving (Eq)

-- |matrix is syntatic sugar to create Matrix
matrix :: [[a]] -> Matrix a
matrix = formMat

-- |Matrices is necessary class for Matrix
class Matrices m where
  matForm :: m a -> [[a]]
  formMat :: [[a]] -> m a

instance Matrices Matrix where
  matForm (Matrix (Vector v) r c b)
    | r*c /= length v = error "Matrix Dimension Miss Match!"
    | b = ctake c v
    | otherwise = dtake c v
      where ctake :: Int -> [a] -> [[a]]
            ctake _ [] = []
            ctake n m = take n m : ctake n (drop n m)
            dtake :: Int -> [a] -> [[a]]
            dtake _ [] = []
            dtake n m = [ptake n m r | r <- [0..(length m `div` n - 1)]]
            ptake n v r = [v !! x | x <- idx v, x `mod` (length v `div` n) == r]
            idx v = take (length v) [0..]
  formMat [] = Matrix (Vector []) 0 0 True
  formMat xs = Matrix (Vector (concat xs)) (length xs) (length (head xs)) True

instance Show a => Show (Matrix a) where
  show m = "Matrix " ++ show (matForm m)

instance Functor Matrix where
  fmap f mat = mat { val = fmap f (val mat) }

instance Applicative Matrix where
  pure a = matrix []
  mf <*> mx = mx { val = val mf <*> val mx }

instance Num a => Num (Matrix a) where
  negate m = negate <$> m
  (+) m n = (+) <$> m <*> n
  (*) m n = (*) <$> m <*> n
  fromInteger a = fromInteger <$> matrix [[a]]
  signum m = signum <$> m
  abs m = abs <$> m

instance Fractional a => Fractional (Matrix a) where
  recip m = recip <$> m
  (/) m n = (*) <$> m <*> recip n
  fromRational n = fromRational <$> matrix [[n]]

instance Foldable Matrix where
  foldr _ z (Matrix (Vector []) _ _ _) = z
  foldr f z (Matrix (Vector xs) _ _ _) = foldr f z xs

  foldl _ z (Matrix (Vector []) _ _ _) = z
  foldl f z (Matrix (Vector xs) _ _ _) = foldl f z xs

---------------------------------------------------
-- Operation
---------------------------------------------------
{-|
   (.<ops>) is an operation Vector with Constant.
   Dot means position of Vector.
   Example: a .* 2  = twice whole elements of a
            a .*. b = Dot product
-}
class Functor f => Numeric f where
  (.+) :: Num a => f a -> a -> f a
  (.-) :: Num a => f a -> a -> f a
  (.*) :: Num a => f a -> a -> f a
  (./) :: Fractional a => f a -> a -> f a
  (.^) :: Floating a => f a -> a -> f a
  (.*.) :: Num a => f a -> f a -> a

instance Numeric Vector where
  v .+ n = (+ n) <$> v
  v .- n = (+ negate n) <$> v
  v .* n = (* n) <$> v
  v ./ n = (/ n) <$> v
  v .^ n = (** n) <$> v
  v .*. w = sum $ v * w

instance Numeric Matrix where
  v .+ n = (+ n) <$> v
  v .- n = (+ negate n) <$> v
  v .* n = (* n) <$> v
  v ./ n = (/ n) <$> v
  v .^ n = (** n) <$> v
  v .*. w = sum $ v * w

---- |Matrix Multiplication using Devide and Conquer Algorithm.
--(%*%) :: Num a => Matrix a -> Matrix a -> Matrix a
--_            %*% Vector []    = Vector []
--Vector []    %*% _            = Vector []
--_            %*% Vector [[]]  = Vector [[]]
--Vector [[] ] %*% _            = Vector [[]]
--Vector [[x]] %*% Vector [[y]] = Vector [[x * y]]
--m            %*% n            = (a11 %++% a12) %**% (a21 %++% a22)
-- where
--  (m11, n11) = (bp 1 m, bp 1 n)
--  (m12, n12) = (bp 2 m, bp 2 n)
--  (m21, n21) = (bp 3 m, bp 3 n)
--  (m22, n22) = (bp 4 m, bp 4 n)
--  a11        = (m11 %*% n11) %+% (m12 %*% n21)
--  a12        = (m11 %*% n12) %+% (m12 %*% n22)
--  a21        = (m21 %*% n11) %+% (m22 %*% n21)
--  a22        = (m21 %*% n12) %+% (m22 %*% n22)
--
---- |Block Partitioning
--bp :: Int -> Matrix a -> Matrix a
--bp n m = fromList $ bpMat n (toList m)
--
---- TODO: Multiplicate Inverse Matrix
----(%/%) :: Fractional a => Matrix a -> Matrix a -> Matrix a
----m %/% n = zipWith (/) <$> m <*> n
--
---- Concatenate
---- |(.++.) horizontally concatenate vectors.
--(.++.) :: Vector a -> Vector a -> Vector a
--v .++. w = fromList (toList v ++ toList w)
--
---- |hcat is same as (.++.). It is just julia syntax.
--hcat :: Vector a -> Vector a -> Vector a
--hcat v w = v .++. w
--
---- |(.**.) vertically stack vectors.
--(.**.) :: Vector a -> Vector a -> Matrix a
--v .**. w = fromList (toList v : [toList w])
--
---- |vcat is same as (.**.)
--vcat :: Vector a -> Vector a -> Matrix a
--vcat v w = v .**. w
--
---- |(.:) inserts vector to head of matrix.
--(.:) :: Vector a -> Matrix a -> Matrix a
--v .: m = fromList (toList v : toList m)
--
---- |(%++%) Horizontally concatenate matrices.
--(%++%) :: Matrix a -> Matrix a -> Matrix a
--m %++% n = fromList $ zipWith (++) (toList m) (toList n)
--
---- |(%**%) Vertically concatenate matrices.
--(%**%) :: Matrix a -> Matrix a -> Matrix a
--m %**% n = fromList (toList m ++ toList n)
--
---- |Norm is norm of vector.
--norm :: Floating a => Vector a -> a
--norm v = sqrt $ v .*. v
--
---- Matrix Implementation
---- |transpose is transpose of matrix.
--transpose :: Matrix a -> Matrix a
--transpose = fromList . transposeMat . toList
--
---- |index represents index of matrix of that position.
---- Example: index [[1,2],[3,4]] = [[(0,0), (0,1)], [(1,0),(1,1)]]
--index :: Matrix a -> [[(Int, Int)]]
--index = indexMat . toList
--
---- |det is determinant of matrix.
--det :: Num a => Matrix a -> a
--det m | isSquare m = (detMat . toList) m
--      | otherwise  = error "It's not Square matrix"
--
---- |isSquare
--isSquare :: Matrix a -> Bool
--isSquare m = all (== length m) (length <$> m)
--
---- |isInvertible
--isInvertible :: (Eq a, Num a) => Matrix a -> Bool
--isInvertible m = det m /= 0
--
---- |inv is inverse of matrix.
--inv :: (Eq a, Fractional a) => Matrix a -> Matrix a
--inv m | isInvertible m = (fromList . invMat . toList) m
--      | otherwise      = error "Matrix is not invertible!"

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
