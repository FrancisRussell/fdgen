module FDGEN.Precedence (PDoc(..), Precedence(..), Assoc(..), pAssoc, pPrec, pDoc
                        , renderTerminal, renderPrefix, renderInfix, renderPrefixMultiParam) where
import Text.PrettyPrint (Doc, hcat, parens)
import Control.Applicative ((<$>))
import FDGEN.Pretty (PrettyPrintable(..))
import Data.List (intersperse)

data Precedence
 = PrecLevel Int
 | AtomPrec
 deriving (Eq, Ord, Show)

data Assoc
 = LeftAssoc
 | RightAssoc
 | NoAssoc
 deriving (Eq, Show)

pAssoc :: PDoc -> Assoc
pAssoc (PDoc _ a _) = a

pPrec :: PDoc -> Precedence
pPrec (PDoc _ _ p) = p

pDoc :: PDoc -> Doc
pDoc (PDoc d _ _) = d

data PDoc = PDoc Doc Assoc Precedence

hasAssociativity :: Assoc -> Bool
hasAssociativity assoc = case assoc of
  LeftAssoc -> True
  RightAssoc -> True
  NoAssoc -> False

renderTerminal :: PrettyPrintable a => a -> PDoc
renderTerminal t = PDoc (toDoc t) NoAssoc AtomPrec

renderPrefix :: PrettyPrintable a => (a, Precedence) -> PDoc -> PDoc
renderPrefix (op, prec) expr = PDoc rendered NoAssoc prec
  where
  rendered = hcat [toDoc op, doBracketing prec NoAssoc expr]

renderPrefixMultiParam :: PrettyPrintable a => (a, Precedence) -> [PDoc] -> PDoc
renderPrefixMultiParam (op, prec) params = PDoc rendered NoAssoc prec
  where
  rendered = hcat $ intersperse (toDoc " ") ((toDoc op):params')
  params' = doBracketing prec NoAssoc <$> params

renderInfix :: PrettyPrintable a => (a, Precedence, Assoc) -> PDoc -> PDoc -> PDoc
renderInfix (op, prec, assoc) left right = PDoc resultDoc assoc prec
  where
  doBracketing' = doBracketing prec assoc
  leftDoc = doBracketing' left
  rightDoc = doBracketing' right
  resultDoc = hcat [leftDoc, toDoc op, rightDoc]

doBracketing prec assoc term = if pPrec term > prec || pPrec term == prec && pAssoc term == assoc && hasAssociativity assoc
  then pDoc term
  else parens $ pDoc term
