module Main where

import Vector
import Stats

main :: IO ()
main = do
    print $ Vector [1,2,3]
    print $ mean (Vector [1,2,3])
