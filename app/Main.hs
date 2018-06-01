module Main where

import           Vector
import           Stats
import           CSV

main :: IO ()
main = do
  let a = Vector [1, 3, 4]
  print a
  print $ map ($ a) [mean, var, std]

