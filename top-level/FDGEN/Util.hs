module FDGEN.Util (mergeBoundingRange, isTerminating) where

import Control.Applicative ((<$>))
import Data.Ratio (denominator)
import Math.NumberTheory.Primes.Factorisation (factorise)
import qualified Data.Set as Set

mergeBoundingRange :: [(Integer, Integer)] -> [(Integer, Integer)] -> [(Integer, Integer)]
mergeBoundingRange a b = mergeBoundingRange' <$> zip a b
  where
  mergeBoundingRange' ((l, u), (l', u')) = (min l l', max u u')

isTerminating :: Rational -> Bool
isTerminating r = checkFactors $ denominator r
  where
  checkFactors i = (Set.fromList $ fst <$> factorise i) `Set.isSubsetOf` (Set.fromList [5, 2])
