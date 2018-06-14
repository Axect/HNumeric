module HNum.Special where

import           System.Random
import qualified Numeric.SpecFunctions         as SF
import           HNum.Vector
import           HNum.F

class FuncTools f => SpecialFunc f where
  erf :: Real a => f a -> f Double
  invErf :: Real a => f a -> f Double

instance SpecialFunc Vector where
  erf v = SF.erf . realToFrac <$> v
  invErf v = SF.invErf . realToFrac <$> v


