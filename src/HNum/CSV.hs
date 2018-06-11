{-|
Module      : HNum.CSV
Description : Simple CSV Library for HNumeric
CopyRight   : (c) Tae Geun Kim, 2018
License     : BSD3
Maintainer  : edeftg@gmail.com
Stability   : Experimental
-}
module HNum.CSV where

import           HNum.Vector

-- | Type Aliases for convenience
type Header = [String]

-- | DataFrame structure to write csv
data DataFrame a = DataFrame { header :: Header, dat :: Matrix a} deriving (Show, Eq)

-- | dataframe constructor
dataframe :: Header -> Matrix a -> DataFrame a
dataframe h m | length h == row m = DataFrame h m
              | otherwise         = error "Length of Header != Length of Data"
  where n = length m `div` length h

-- | dataframe from vectors
fromVectors :: Header -> [Vector a] -> DataFrame a
fromVectors h vs = dataframe h (matrix vs') where vs' = map toList vs

instance Functor DataFrame where
  fmap f df = df { dat = fmap f (dat df) }

-- | Class to write csv file
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
  toString (DataFrame h m) = h' ++ "\n" ++ m'
    where h' = cm h
          m' = toString (transpose m)
  write title df = writeFile title (toString df)


-- | Convenient 
cm :: [String] -> String
cm [] = []
cm (x : xs) | null xs   = x
            | otherwise = x ++ "," ++ cm xs
