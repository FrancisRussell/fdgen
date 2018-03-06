module FDGEN.Precedence (PDoc(..), Precedence(..), Assoc(..), pAssoc, pPrec, pDoc
                        , renderTerminal, renderPrefix, renderInfix) where
import Text.PrettyPrint (Doc, hcat, parens)
import FDGEN.Pretty (PrettyPrintable(..))

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

renderTerminal :: PrettyPrintable a => a -> PDoc
renderTerminal t = PDoc (toDoc t) NoAssoc AtomPrec

renderPrefix :: PrettyPrintable a => (a, Precedence) -> PDoc -> PDoc
renderPrefix (op, prec) expr = PDoc rendered NoAssoc prec
  where
  rendered = hcat [toDoc op, doBracketing prec NoAssoc expr]

renderInfix :: PrettyPrintable a => (a, Precedence, Assoc) -> PDoc -> PDoc -> PDoc
renderInfix (op, prec, assoc) left right = PDoc resultDoc assoc prec
  where
  doBracketing' = doBracketing prec assoc
  leftDoc = doBracketing' left
  rightDoc = doBracketing' right
  resultDoc = hcat [leftDoc, toDoc op, rightDoc]

doBracketing prec assoc term = if pPrec term > prec || pPrec term == prec && pAssoc term == assoc
  then pDoc term
  else parens $ pDoc term
