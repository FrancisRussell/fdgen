{-# LANGUAGE TemplateHaskell, EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
module FDGEN.Algebra (Expression(..), subst, lagrange) where
import Data.Map.Strict (Map)
import Data.Ratio ((%), denominator, numerator)
import Data.Foldable (foldl')
import FDGEN.Pretty (PrettyPrintable(..))
import Text.PrettyPrint (Doc, hcat, char, parens, punctuate)
import Control.Applicative ((<$>))
import qualified Data.Map as Map
import qualified Control.Lens as Lens

data SumTag

data ProdTag

data Expression e
  = Symbol e
  | Sum (PairSeq SumTag e)
  | Product (PairSeq ProdTag e)
  | ConstantFloat Double
  | ConstantRational Rational
  | Abs (Expression e)
  | Signum (Expression e)
  deriving (Show, Eq, Ord)

data RewriteState
  = NonNormal
  | Normal
  | NormalAndExtracted
  deriving (Eq, Ord, Show)

data PairSeq t e = PairSeq {
  _psOverall :: Rational,
  _psTerms :: Map (Expression e) Rational,
  _psRewriteState :: RewriteState
} deriving (Eq, Ord, Show)

Lens.makeLenses ''PairSeq

class PairSeqLike t e where
  empty :: PairSeq t e
  extractMultipliers :: Ord e => PairSeq t e -> PairSeq t e
  wrap :: PairSeq t e -> Expression e
  addTerm :: Ord e => Expression e -> Rational -> PairSeq t e -> PairSeq t e

isNormalised :: PairSeq t e -> Bool
isNormalised seq' = case _psRewriteState seq' of
  Normal -> True
  NormalAndExtracted -> True
  _ -> False

normalise :: (PairSeqLike t e, Ord e) => PairSeq t e -> PairSeq t e
normalise seq' = if isNormalised seq'
  then seq'
  else Lens.set psRewriteState Normal . removeZeros $ extractMultipliers seq'

extractMultiplier :: forall e . Ord e => Expression e -> (Rational, Expression e)
extractMultiplier (Product seq') = case _psRewriteState seq' of
  NormalAndExtracted -> (1, simplifyPairSeq seq')
  _ -> (overall, simplifyPairSeq updated)
       where
       normalised = normalise seq'
       overall = _psOverall normalised
       updated :: PairSeq ProdTag e
       updated = normalised { _psOverall = 1, _psRewriteState = NormalAndExtracted }
extractMultiplier (Sum seq') = case _psRewriteState seq' of
  NormalAndExtracted -> (1, simplifyPairSeq seq')
  _ -> (common, simplifyPairSeq updated)
       where
       normalised = normalise seq'
       overall = _psOverall normalised
       terms = _psTerms normalised
       gcd' r1 r2 = case (r1, r2) of
         (0, 0) -> 1
         (_, 0) -> abs r1
         (0, _) -> abs r2
         (_, _) -> (gcd (numerator r1) (numerator r2)) % (gcd (denominator r1) (denominator r2))
       coeffs = overall:(Map.elems terms)
       moreNegative = (length $ filter ( < 0) coeffs) > div (length coeffs) 2
       sign = if moreNegative then (-1) else 1
       common = sign * foldl' gcd' 0 coeffs
       overall' = (_psOverall normalised) / common
       terms' = Map.map (/ common) $ _psTerms normalised
       updated :: PairSeq SumTag e
       updated = PairSeq { _psOverall = overall', _psRewriteState = NormalAndExtracted, _psTerms = terms' }
extractMultiplier e = (1, simplify e)

instance PairSeqLike SumTag e where
  empty = PairSeq {
    _psOverall = 0,
    _psTerms = Map.empty,
    _psRewriteState = NonNormal
  }
  extractMultipliers seq' = Map.foldlWithKey addScaledTerm base (_psTerms seq')
    where
    base = (empty :: PairSeq SumTag e) { _psOverall = _psOverall seq' }
    addScaledTerm :: PairSeq SumTag e -> Expression e -> Rational -> PairSeq SumTag e
    addScaledTerm ps expr coeff = addTerm expr' (coeff * multiplier) ps
      where
      (multiplier, expr') = extractMultiplier expr
  wrap = Sum
  addTerm expr coeff seq' = case expr of
      ConstantRational r -> Lens.over psOverall (+ (r * coeff)) seq'
      Sum subSeq -> foldl' addTerm' seq' (asPairs subSeq)
       where
       addTerm' s (e, c) = addTerm e (c * coeff) s
      _ -> normalInsert
      where
      normalInsert = incrementTerm expr coeff seq'

instance PairSeqLike ProdTag e where
  empty = PairSeq {
    _psOverall = 1,
    _psTerms = Map.empty,
    _psRewriteState = NonNormal
  }
  extractMultipliers seq' = Map.foldlWithKey addScaledTerm base (_psTerms seq')
    where
    base = (empty :: PairSeq ProdTag e) { _psOverall = _psOverall seq' }
    addScaledTerm :: PairSeq ProdTag e -> Expression e -> Rational -> PairSeq ProdTag e
    addScaledTerm ps expr coeff = newSeq
      where
      (multiplier, expr') = extractMultiplier expr
      isIntegerCoeff = denominator coeff == 1
      newSeq = if isIntegerCoeff
        then foldl' (flip $ uncurry addTerm) ps [(ConstantRational multiplier, coeff), (expr', coeff)]
        else addTerm expr coeff ps
  wrap = Product
  addTerm expr coeff seq' = newSeq
    where
    isIntegerCoeff = denominator coeff == 1
    newSeq = if isIntegerCoeff
      then case expr of
        ConstantRational r -> Lens.over psOverall (* (r ^^ numerator coeff)) seq'
        Product subSeq -> foldl' addTerm' seq' (asPairs subSeq)
         where
         addTerm' s (e, c) = addTerm e (c * coeff) s
        _ -> normalInsert
      else normalInsert
      where
      normalInsert = incrementTerm expr coeff seq'

incrementTerm :: Ord e => Expression e -> Rational -> PairSeq t e -> PairSeq t e
incrementTerm expr coeff = Lens.over psTerms (Map.insertWith (+) expr coeff)

asPairs :: PairSeqLike t e => PairSeq t e -> [(Expression e, Rational)]
asPairs seq' = if hasNullOverall seq'
  then terms
  else overall:terms
  where
  overall = (ConstantRational $ _psOverall seq', 1)
  terms = Map.assocs $ _psTerms seq'

removeZeros :: PairSeq t e -> PairSeq t e
removeZeros seq' = seq' { _psTerms = terms' }
  where
  terms' = Map.filter (/= 0) $ _psTerms seq'

add :: Ord e => Expression e -> Expression e -> Expression e
add a b = Sum $ foldl' (flip $ uncurry addTerm) empty [(a, 1), (b, 1)]

sub :: Ord e => Expression e -> Expression e -> Expression e
sub a b = Sum $ foldl' (flip $ uncurry addTerm) empty [(a, 1), (b, -1)]

mul :: Ord e => Expression e -> Expression e -> Expression e
mul a b = Product $ foldl' (flip $ uncurry addTerm) empty [(a, 1), (b, 1)]

divide :: Ord e => Expression e -> Expression e -> Expression e
divide a b = Product $ foldl' (flip $ uncurry addTerm) empty [(a, 1), (b, -1)]

hasNullOverall :: forall t e . PairSeqLike t e => PairSeq t e -> Bool
hasNullOverall seq' = (_psOverall seq') == (_psOverall (empty :: PairSeq t e))

simplifyPairSeq :: (PairSeqLike t e, Ord e) => PairSeq t e -> Expression e
simplifyPairSeq seq' = newExpr
  where
  normalised = normalise seq'
  newExpr = case Map.toList (_psTerms normalised) of
    [] -> ConstantRational $ _psOverall normalised
    [(a, 1)] -> if (hasNullOverall normalised)
      then a
      else wrap normalised
    _ -> wrap normalised

simplify :: Ord e => Expression e -> Expression e
simplify (Product seq') = simplifyPairSeq seq'
simplify (Sum seq') = simplifyPairSeq seq'
simplify (Abs e) = simplify' . Abs $ simplify e
simplify (Signum e) = simplify' . Signum $ simplify e
simplify e = simplify' e

simplify' :: Ord e => Expression e -> Expression e
simplify' (Abs (ConstantRational r)) = ConstantRational $ abs r
simplify' (Abs (ConstantFloat f)) = ConstantFloat $ abs f
simplify' (Signum (ConstantRational r)) = ConstantRational $ signum r
simplify' (Signum (ConstantFloat f)) = ConstantFloat $ signum f
simplify' e = e

rewrite :: Ord e => (Expression e -> Expression e) -> Expression e -> Expression e
rewrite f e = simplify' . f $ rewrite' f e

rewritePairSeq :: (Ord e, PairSeqLike t e) => (Expression e -> Expression e) -> PairSeq t e -> PairSeq t e
rewritePairSeq f seq' = foldl' addTerm' empty $ asPairs seq'
  where
  addTerm' s (e, r) = addTerm (rewrite f e) r s

rewrite' :: Ord e => (Expression e -> Expression e) -> Expression e -> Expression e
rewrite' f (Sum seq') = simplifyPairSeq $ rewritePairSeq f seq'
rewrite' f (Product seq') = simplifyPairSeq $ rewritePairSeq f seq'
rewrite' f (Abs e) = Abs . simplify' $ rewrite f e
rewrite' f (Signum e) = Signum . simplify' $ rewrite f e
rewrite' _ e@(Symbol _) = e
rewrite' _ e@(ConstantFloat _) = e
rewrite' _ e@(ConstantRational _) = e

subst :: Ord e => Expression e -> Expression e -> Expression e -> Expression e
subst from to e = simplify $ rewrite update e
  where
  update e' = if e' == from then to else e'

lagrange :: Ord e => Expression e -> [(Expression e, Expression e)] -> Expression e
lagrange sym points  = foldl' (+) 0 bases
  where
  bases = [constructBasis j | j <- [0 .. (length points - 1)]]
  constructBasis j = foldl' (*) yj $ map term pointIndices
    where
      (xj, yj) = points !! j
      pointIndices = filter (/=j) [0 .. (length points - 1)]
      term k = (sym - xk) / (xj - xk)
        where
          (xk, _) = points !! k

instance Ord e => Num (Expression e) where
  fromInteger = ConstantRational . fromInteger
  (+) a = simplify . add a
  (-) a = simplify . sub a
  (*) a = simplify . mul a
  abs = simplify . Abs
  signum = simplify . Signum

instance Ord e => Fractional (Expression e) where
  fromRational = ConstantRational
  (/) a = simplify . divide a

data Precedence
 = AddPrec
 | MulPrec
 | PowPrec
 | TerminalPrec
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

instance PrettyPrintable e => PrettyPrintable (Expression e) where
  toDoc expr = pDoc $ toPDoc expr
    where
    renderTerminal :: PrettyPrintable a => a -> PDoc
    renderTerminal t = PDoc (toDoc t) NoAssoc TerminalPrec
    renderInteger :: Integer -> PDoc
    renderInteger i = if i >= 0
      then PDoc (toDoc i) NoAssoc TerminalPrec
      else PDoc (hcat [char '-', toDoc (-i)]) NoAssoc AddPrec
    renderRational r = case denominator r of
       1 -> renderInteger $ numerator r
       _ -> renderDivision (renderInteger $ numerator r) (renderInteger $ denominator r)
    renderBinary :: String -> Assoc -> Precedence -> PDoc -> PDoc -> PDoc
    renderBinary op assoc prec left right = PDoc resultDoc assoc prec
      where
      doBracketing term = if pPrec term > prec || pPrec term == prec && pAssoc term == assoc
        then pDoc term
        else parens $ pDoc term
      leftDoc = doBracketing left
      rightDoc = doBracketing right
      resultDoc = hcat [leftDoc, toDoc op, rightDoc]
    renderDivision = renderBinary "/" LeftAssoc MulPrec
    renderMultiplication = renderBinary "*" LeftAssoc MulPrec
    renderAddition = renderBinary "+" LeftAssoc AddPrec
    renderPower = renderBinary "^" RightAssoc PowPrec
    renderPairSeq :: (PairSeqLike t c, PrettyPrintable c) => PairSeq t c -> (PDoc -> PDoc -> PDoc) -> (PDoc -> PDoc -> PDoc) -> PDoc
    renderPairSeq seq' renderPair combineTerms = if Map.null $ _psTerms seq'
      then renderRational $ _psOverall seq'
      else foldl' combineTerms (head renderedTerms) (tail renderedTerms)
      where
      base = if hasNullOverall seq'
        then []
        else [renderRational $ _psOverall seq']
      renderPair' (e, c) = if c == 1
        then toPDoc e
        else renderPair (toPDoc e) (renderRational c)
      renderedTerms = (renderPair' <$> (Map.assocs $ _psTerms seq')) ++ base
    renderFunction :: String -> [PDoc] -> PDoc
    renderFunction name args = renderTerminal call
      where
      call = hcat [toDoc name, parens . hcat . punctuate (toDoc ", ") $ pDoc <$> args]
    toPDoc expr' = case expr' of
      Symbol s -> renderTerminal s
      ConstantFloat f -> renderTerminal f
      ConstantRational r -> renderRational r
      Sum seq' -> renderPairSeq seq' renderMultiplication renderAddition
      Product seq' -> renderPairSeq seq' renderPower renderMultiplication
      Abs e -> renderFunction "abs" [toPDoc e]
      Signum e -> renderFunction "signum" [toPDoc e]
