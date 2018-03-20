module FDGEN.Util (mergeBoundingRange) where

import Control.Applicative ((<$>))

mergeBoundingRange :: [(Integer, Integer)] -> [(Integer, Integer)] -> [(Integer, Integer)]
mergeBoundingRange a b = mergeBoundingRange' <$> zip a b
  where
  mergeBoundingRange' ((l, u), (l', u')) = (min l l', max u u')


