module HNum.Dist where

import           System.Random
import qualified Numeric.SpecFunctions         as SF
import           HNum.Vector
import           HNum.F

class FuncTools f => Special f where
  erf :: Real a => f a -> f Double

instance Special Vector where
  erf v = SF.erf . realToFrac <$> v

