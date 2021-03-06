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
import           Data.List                      ( intercalate )
import           Control.Parallel
import           Control.Concurrent.ParallelIO
import qualified Control.Concurrent.ParallelIO.Local
                                               as Local

-----------------------------------------------
-- Declaration
-----------------------------------------------
-- | Type Aliases for convenience
type Header = [String]

-- | DataFrame structure to write csv
data DataFrame a = DataFrame { header :: Header
                             , dat :: Matrix a -- ^ Row Based
                             } deriving (Show, Eq)

-- | dataframe constructor
dataframe :: Header -> Matrix a -> DataFrame a
dataframe h m | length h == row m = DataFrame h m
              | otherwise         = error "Length of Header != Length of Data"
  where n = length m `div` length h

-- | dataframe from vectors
fromVectors :: Header -> [Vector a] -> DataFrame a
fromVectors h vs = dataframe h (matrix vs') where vs' = map toList vs

-- | Extract Vector in DataFrame
(#) :: Eq a => DataFrame a -> String -> Vector a
df # hd | hd `notElem` header df = error "No specific header in dataFrame"
        | otherwise              = i `par` bd `pseq` vec $ bd !! i
 where
  i  = hd `fd` header df
  bd = matForm $ dat df

instance Functor DataFrame where
  fmap f df = df { dat = fmap f (dat df) }

instance Convertable DataFrame where
  readInt df = read <$> df :: DataFrame Int
  readInteger df = read <$> df :: DataFrame Integer
  readDouble df = read <$> df :: DataFrame Double

-----------------------------------------------
-- Write to CSV
-----------------------------------------------
-- | Class to write csv file
class Functor f => Writable f where
  -- | Object to String (Different to Show)
  toString :: Show a => f a -> String
  -- | Write as CSV
  writeCSV :: (Eq a, Show a) => String -> f a -> IO ()

instance Writable Vector where
  toString v = intercalate "\n" (toList $ show <$> v)
  writeCSV title v = writeFile title (toString v)

instance Writable Matrix where
  toString m = m1 `seq` m2 `seq` intercalate "\n" m2
    where m1 = matForm (show <$> m)
          m2 = map (intercalate ",") m1
  writeCSV title m = writeFile title (toString m)

instance Writable DataFrame where
  toString = undefined
  writeCSV parentPath df =
    let childPath = header df
        vecValue = [df # heads | heads <- childPath]
        strValue = [toString vecs | vecs <- vecValue]
        candidate = [heads ++ "\n" ++ strs | heads <- childPath, strs <- strValue]
        commands = [writeFile (parentPath ++ "/" ++ heads ++ ".csv") cds | heads <- childPath, cds <- candidate]
    in childPath
        `pseq` vecValue
        `pseq` strValue
        `pseq` candidate
        `pseq` commands
        `pseq` do
          Local.withPool 4 $ \pool -> Local.parallel_ pool commands



-----------------------------------------------
-- Read from CSV
-----------------------------------------------
-- | From CSV to DataFrame
readCSV :: String -> IO (DataFrame String)
readCSV filepath = do
  r <- readFile filepath
  let dat = splitWith '\n' (rmQuot r)
      hd  = splitWith ',' (head dat)
      bd  = map (splitWith ',') (tail dat)
  return DataFrame {header = hd, dat = matrix bd}

-----------------------------------------------
-- Backend Function
-----------------------------------------------
-- | Split With Seperator
splitWith :: Char -> String -> [String]
splitWith _ [] = []
splitWith sep str
  | null temp'' = [temp]
  | otherwise = temp `par` temp'' `pseq` temp' `pseq` temp : splitWith sep temp'
 where
  temp   = takeWhile (/= sep) str
  temp'' = dropWhile (/= sep) str
  temp'  = tail temp''

-- | Remove Quotation Symbol
rmQuot :: String -> String
rmQuot [] = []
rmQuot x
  | null temp = clean
  | otherwise = clean `par` temp `pseq` clean' `pseq` clean ++ rmQuot clean'
 where
  clean  = takeWhile (/= '"') x
  temp   = dropWhile (/= '"') x
  clean' = tail temp

