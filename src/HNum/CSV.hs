module HNum.CSV where

import           HNum.Vector
import qualified Data.Map.Strict               as M

type Header = String

data DataFrame a = Map Header (Vector a) deriving (Show, Eq)

class Functor f => CSVtize f where
  toString :: Show a => f a -> String
  write :: Show a => String -> f a -> IO ()

instance CSVtize Vector where
  toString v = foldr (\x y -> x ++ "\n" ++ y) "" (show <$> v)
  write title v = writeFile title (toString v)
