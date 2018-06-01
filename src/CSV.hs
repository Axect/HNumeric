module CSV where

import           Vector

data DataFrame = DataFrame { header :: Vector String
                           , numeric :: Matrix Double
                           } deriving (Eq, Show)

type FilePath = String

-- String to [String] With separator 
splitWith :: Char -> String -> [String]
splitWith _ "" = []
splitWith c s | null ms   = [ws]
              | otherwise = ws : splitWith c (tail ms)
 where
  ws = takeWhile (/= c) s
  ms = dropWhile (/= c) s

-- CSV to DataFrame
-- Deprecated.
fromCSV :: String -> DataFrame
fromCSV r = DataFrame {header = Vector h1, numeric = Vector v1}
 where
  q1 = filter (/= '"') r
  q2 = splitWith '\n' q1
  q3 = map (splitWith ',') q2
  h1 = head q3
  v1 = map (map read) (tail q3) :: [[Double]]

-- DataFrame to 
--writeCSV :: DataFrame -> FilePath -> IO ()
--writeCSV df f = 
--  where q1 = 
