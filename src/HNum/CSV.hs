module HNum.CSV where

import           HNum.Vector
--import qualified Data.Map.Strict               as M

type Header = String

data DataFrame a = DataFrame { header :: Vector String, dat :: Matrix a, lab :: Vector String } deriving (Show, Eq)

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
          cm :: [String] -> String
          cm [] = []
          cm (x:xs) | null xs = x
                    | otherwise = x ++ "," ++ cm xs
  write title m = writeFile title (toString m)

