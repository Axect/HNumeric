import Data.List.Split

main :: IO ()
main = do
  let a = ones 100
  print $ sum $ map sum (mul' a a)

type Vector a = [a]
type Matrix a = [[a]]

matForm :: (Eq a, Show a) => [a] -> IO ()
matForm (row:rows)
  | rows /= [] = do
    putStrLn (show row)
    matForm rows
  | otherwise = putStrLn (show row)

mul :: Num a => Matrix a -> Matrix a -> Matrix a 
mul a b = map (\x -> map (foldr (+) 0) (map (zipWith (*) x) (transpose b))) a

mul' a b = chunksOf (length a) $ do
  x <- a
  y <- transpose b
  [sum (zipWith (*) x y)]

transpose :: [[a]] -> [[a]]
transpose m = map (\l -> map (!! l) m) [0 .. (length m - 1)]

ones n = do
  x <- replicate n 1
  [replicate n x]

