{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module FDGEN.Pretty (PrettyPrintable(..), structureDoc, hListDoc
                    , vListDoc, hPairDoc) where

import Control.Applicative ((<$>))
import Text.PrettyPrint ( Doc, renderStyle, Style(..), Mode(..), text
                        , double, float, integer, ($+$), nest, empty
                        , hsep, (<>), punctuate, hcat, int)

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

instance PrettyPrintable Int where
  toDoc = int

instance PrettyPrintable Bool where
  toDoc = text . show

instance PrettyPrintable Rational where
  toDoc = double . fromRational

structureDoc :: String -> [(String, Doc)] -> Doc
structureDoc name fields = text name $+$ text "{" $+$ nest 2 fieldsDoc $+$ text "}"
  where
  fieldsDoc = foldl ($+$) empty $ punctuate (text ",") $ renderField <$> fields
  renderField (fname, doc) = hsep [text fname, text "=", doc]

hListDoc :: PrettyPrintable e => [e] -> Doc
hListDoc elements = text "[" <> (hcat $ punctuate (text ", ") (toDoc <$> elements)) <> text "]"

hPairDoc :: (PrettyPrintable a, PrettyPrintable b) => (a, b) -> Doc
hPairDoc (a, b) = text "(" <> toDoc a <> text ", " <> toDoc b <> text "]"

vListDoc :: PrettyPrintable e => [e] -> Doc
vListDoc elements = foldl ($+$) empty $ (indent $ toDoc <$> elements) ++ [text "]"]
  where
  indent (d:ds) = (indent' "[" d):(map (indent' ",") ds)
  indent [] = [text "["]
  indent' prefix content = hcat [text prefix <> text " ", content]
