{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module FDGEN.Pretty (PrettyPrintable(..)) where

import Text.PrettyPrint (Doc, renderStyle, Style(..), Mode(..), text, double, float, integer)

class PrettyPrintable a where
  toDoc :: a -> Doc
  prettyPrint :: a -> String
  prettyPrint = renderStyle Style {mode = PageMode, lineLength=72, ribbonsPerLine=1.5 } . toDoc

instance PrettyPrintable Doc where
  toDoc = id

instance PrettyPrintable String where
  toDoc = text

instance PrettyPrintable Double where
  toDoc = double

instance PrettyPrintable Float where
  toDoc = float

instance PrettyPrintable Integer where
  toDoc = integer
