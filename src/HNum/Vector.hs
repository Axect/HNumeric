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
    | r*c /= length v = error "Matrix Dimension mismatch!"
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
   (.<ops>) is an operation Vector(or Matrix) with Constant.
   Dot means position of Vector.
   Example: a .* 2  = twice whole elements of a
            a .*. b = Dot product
-}
class Functor f => VecOps f where
  (.+) :: Num a => f a -> a -> f a
  (.-) :: Num a => f a -> a -> f a
  (.*) :: Num a => f a -> a -> f a
  (./) :: Fractional a => f a -> a -> f a
  (.^) :: Floating a => f a -> a -> f a
  (.*.) :: Num a => f a -> f a -> a

{-
   MatOps is just additional operations for Matrices.
-}
class Functor f => MatOps f where
  (%*%) :: Num a => f a -> f a -> f a
  (%/%) :: Fractional a => f a -> f a -> f a
  det :: Fractional a => f a -> a
  inv :: Fractional a => f a -> f a
  transpose :: f a -> f a

instance VecOps Vector where
  v .+ n = (+ n) <$> v
  v .- n = (+ negate n) <$> v
  v .* n = (* n) <$> v
  v ./ n = (/ n) <$> v
  v .^ n = (** n) <$> v
  v .*. w = sum $ v * w

instance VecOps Matrix where
  v .+ n = (+ n) <$> v
  v .- n = (+ negate n) <$> v
  v .* n = (* n) <$> v
  v ./ n = (/ n) <$> v
  v .^ n = (** n) <$> v
  v .*. w = sum $ v * w

instance MatOps Matrix where
  m %*% n | col m /= row n = error "Can't Multiply - Dimension mismatch!"
          | otherwise      = matrix $ matForm m %-*-% matForm n
  m %/% n = m %*% inv n
  det m | col m /= row m = error "Can't calculate determinant of non-square matrix"
        | otherwise = detMat (matForm m)
  inv m | col m /= row m = error "Can't calculate inverse of non-square matrix"
        | otherwise = (matrix . invMat . matForm) m
  transpose m = m {byRow = not (byRow m)}

-- |Block Partitioning
bp :: Int -> Matrix a -> Matrix a
bp n m = matrix $ bpMat n (matForm m)

---------------------------------------------------
-- Concatenate
---------------------------------------------------
class Functor f => Concatable f where
  hcat :: f a -> f a -> f a
  vcat :: f a -> f a -> Matrix a

instance Concatable Vector where
  hcat v w = fromList (toList v ++ toList w)
  vcat v w = matrix (toList v : [toList w])

instance Concatable Matrix where
  hcat m n | row m == row n = matrix (zipWith (++) mf nf)
           | otherwise = error "Can't concatenate matrices horizontally which have different row"
           where mf = matForm m
                 nf = matForm n
  vcat m n | col m == col n = m {val = hcat (val m) (val n), row = row m + row n}
           | otherwise = error "Can't concatenate matrices vertically which have different col"

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

---------------------------------------------------
-- Useful Functions
---------------------------------------------------
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

-- Matrix + Matrix
(%-+-%) :: Num a => [[a]] -> [[a]] -> [[a]]
m    %-+-% []   = m
[]   %-+-% m    = m
[[]] %-+-% m    = m
m    %-+-% [[]] = m
m    %-+-% n    = zipWith (zipWith (+)) m n

negMap :: Num a => [[a]] -> [[a]]
negMap = map (map negate)

-- Matrix - Matrix
(%---%) :: Num a => [[a]] -> [[a]] -> [[a]]
m    %---% []   = m
[]   %---% m    = map (map negate) m
[[]] %---% m    = map (map negate) m
m    %---% [[]] = m
m    %---% n    = zipWith (zipWith (-)) m n

-- Matrix Multiplication
(%-*-%) :: Num a => [[a]] -> [[a]] -> [[a]]
_     %-*-% []    = []
[]    %-*-% _     = []
_     %-*-% [[]]  = [[]]
[[] ] %-*-% _     = [[]]
[[x]] %-*-% [[y]] = [[x * y]]
m     %-*-% n     = zipWith (++) a11 a12 ++ zipWith (++) a21 a22
 where
  (m11, n11) = (bpMat 1 m, bpMat 1 n)
  (m12, n12) = (bpMat 2 m, bpMat 2 n)
  (m21, n21) = (bpMat 3 m, bpMat 3 n)
  (m22, n22) = (bpMat 4 m, bpMat 4 n)
  a11        = (m11 %-*-% n11) %-+-% (m12 %-*-% n21)
  a12        = (m11 %-*-% n12) %-+-% (m12 %-*-% n22)
  a21        = (m21 %-*-% n11) %-+-% (m22 %-*-% n21)
  a22        = (m21 %-*-% n12) %-+-% (m22 %-*-% n22)

zerosVec :: Int -> [Int]
zerosVec n = take n [0, 0 ..]

eyeMat :: Int -> [[Int]]
eyeMat n = [ basisVec x n | x <- [0 .. (n - 1)] ]

-- Position -> Length 
basisVec :: Int -> Int -> [Int]
basisVec n m = zerosVec n ++ [1] ++ zerosVec (m - n - 1)

permMat :: Int -> Int -> [[a]] -> [[Int]]
permMat i j m
  | i < j
  = take i idx
    ++ [idx !! j]
    ++ take (j - i - 1) (drop (i + 1) idx)
    ++ [idx !! i]
    ++ drop (j + 1) idx
  | otherwise
  = permMat j i m
  where idx = eyeMat (length m)

whichMax :: Ord a => [a] -> Int
whichMax v = whichMax' v 0 m
 where
  m = maximum v
  whichMax' :: Ord a => [a] -> Int -> a -> Int
  whichMax' (x : xs) n m' = if x == m' then n else whichMax' xs (n + 1) m'

colMat :: [[a]] -> Int -> [a]
colMat m n = map (!! n) m

colMaxIdx :: Ord a => [[a]] -> Int -> Int
colMaxIdx m n = whichMax $ colMat m n

-- | Another Block Partitioning
bpMat' :: Int -> [[a]] -> [[a]]
bpMat' _ []  = []
bpMat' _ [x] = [x]
bpMat' n m | n == 1 = (map (take l) . take l) m
           | n == 2 = (map (drop 1) . take l) m
           | n == 3 = (map (take l) . drop 1) m
           | n == 4 = (map (drop 1) . drop 1) m
           | n == 0 = (map (drop 1 . take l) . drop 1 . take l) m
  where l = length m - 1

-- | Determinant for Double List - Order ~ 4^n
detMat :: Fractional a => [[a]] -> a
detMat [[x]] = x
detMat m
  | l == 2    = detMat m11 * detMat m22 - detMat m12 * detMat m21
  | otherwise = (detMat m11 * detMat m22 - detMat m12 * detMat m21) / detMat m00
 where
  l   = length m
  m11 = bpMat' 1 m
  m12 = bpMat' 2 m
  m21 = bpMat' 3 m
  m22 = bpMat' 4 m
  m00 = bpMat' 0 m

-- | Inverse for Double List - Order ~ n * 2^n
invMat :: Fractional a => [[a]] -> [[a]]
invMat []    = []
invMat [[] ] = [[]]
invMat [[x]] = [[x]]
invMat m
  | length m == 2
  = map (map (/ detMat m))
    $  zipWith (++) m22          (negMap m12)
    ++ zipWith (++) (negMap m21) m11
  | otherwise
  = zipWith (++) a11 a12 ++ zipWith (++) a21 a22
 where
  m11 = bpMat 1 m
  m12 = bpMat 2 m
  m21 = bpMat 3 m
  m22 = bpMat 4 m
  a00 = invMat m11
  s   = m22 %---% (m21 %-*-% a00 %-*-% m12)
  s00 = invMat s
  a11 = a00 %-+-% (a00 %-*-% m12 %-*-% s00 %-*-% m21 %-*-% a00)
  a12 = negMap a00 %-*-% m12 %-*-% s00
  a21 = negMap s00 %-*-% m21 %-*-% a00
  a22 = s00
