module HNum.CSV where

import           HNum.Vector

type Header = [String]
type Label = [String]

data DataFrame a = DataFrame { header :: Header, dat :: Matrix a, lab :: Label} deriving (Show, Eq)

-- | No label dataframe
dataframe :: Header -> Matrix a -> DataFrame a
dataframe h m | length h == row m = DataFrame h m (replicate n "value")
              | otherwise         = error "Length of Header != Length of Data"
  where n = length m `div` length h

-- | With label dataframe
dataframe' :: Header -> Matrix a -> Label -> DataFrame a
dataframe' h m l
  | length h == row m && row m == length l = DataFrame h m l
  | otherwise = error "Length of Header != Length of Data != Length of Label"

-- | No label dataframe from Vectors
fromVectors :: Header -> [Vector a] -> DataFrame a
fromVectors h vs = dataframe h (matrix vs') where vs' = map toList vs

-- | With label dataframe from Vectors
fromVectors' :: Header -> [Vector a] -> Label -> DataFrame a
fromVectors' h vs = dataframe' h (matrix vs') where vs' = map toList vs

instance Functor DataFrame where
  fmap f df = df { dat = fmap f (dat df) }

class Functor f => CSVtize f where
  toString :: Show a => f a -> String
  write :: Show a => String -> f a -> IO ()

instance CSVtize Vector where
  toString v = foldr (\x y -> x ++ "\n" ++ y) "" (show <$> v)
  write title v = writeFile title (toString v)

instance CSVtize Matrix where
  toString m = foldr ((\x y -> x ++ "\n" ++ y) . cm) "" m1
    where m1 = matForm (show <$> m)
  write title m = writeFile title (toString m)

instance CSVtize DataFrame where
  toString (DataFrame h m l) = h' ++ "\n" ++ m'
    where h' = cm h ++ ",label"
          m' = foldr ((\x y -> x ++ "\n" ++ y) . cm) "" $ matForm $ hcat (show <$> transpose m) l'
          l' = transpose $ matrix [l]
  write title df = writeFile title (toString df)


-- | Convenient 
cm :: [String] -> String
cm [] = []
cm (x : xs) | null xs   = x
            | otherwise = x ++ "," ++ cm xs
