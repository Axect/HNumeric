module Main where

import           HNum.Vector
import           HNum.Stats
import           HNum.CSV
import           Data.Random.Normal

main :: IO ()
main = do
  let a = Vector [1, 3, 4]
  print a
  print $ map ($ a) [mean, var, std]

