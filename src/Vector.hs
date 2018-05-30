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

-- isSquare
isSquare :: Matrix a -> Bool
isSquare m = all (== length m) (length <$> m)

-- isInvertible
isInvertible :: Matrix a -> Bool
isInvertible m = det m /= 0

-- Inverse
inv :: Fractional a => Matrix a -> Matrix a
inv m | isInvertible m = (fromList . invMat . toList) m
      | otherwise      = error "Matrix is not invertible!"




-- Useful Function
-- Transpose
transposeMat :: [[a]] -> [[a]]
transposeMat m = map (\l -> map (!! l) m) [0 .. (length m - 1)]

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
cofactorMat i j = (* (-1) ^ (i + j)) . detMat . (dropAtMat i j)

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
