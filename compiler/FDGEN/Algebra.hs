{-# LANGUAGE TemplateHaskell, EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
module FDGEN.Algebra (Expression(..)) where
import Data.Map (Map)
import Data.Ratio ((%), denominator, numerator)
import qualified Data.Map as Map
import qualified Control.Lens as Lens

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
  _overall :: Rational,
  _terms :: Map (Expression e) Rational,
  _rewriteState :: RewriteState
} deriving (Eq, Ord, Show)

data SumTag

data ProdTag

class PairSeqLike t e where
  empty :: PairSeq t e
  extractMultipliers :: Ord e => PairSeq t e -> PairSeq t e
  wrap :: PairSeq t e -> Expression e
  addTerm :: Ord e => PairSeq t e -> Expression e -> Rational -> PairSeq t e

isNormalised :: PairSeq t e -> Bool
isNormalised seq' = case _rewriteState seq' of
  Normal -> True
  NormalAndExtracted -> True
  _ -> False

normalise :: (PairSeqLike t e, Ord e) => PairSeq t e -> PairSeq t e
normalise seq' = if isNormalised seq'
  then seq'
  else removeZeros $ extractMultipliers seq'

extractMultiplier :: Ord e => Expression e -> (Rational, Expression e)
extractMultiplier (Product seq') = case _rewriteState seq' of
  NormalAndExtracted -> (1, Product seq')
  _ -> (overall, Product updated)
       where
       normalised = normalise seq'
       overall = _overall normalised
       updated = normalised { _overall = 1, _rewriteState = NormalAndExtracted }
extractMultiplier e = (1, e)

instance PairSeqLike SumTag e where
  empty = PairSeq {
    _overall = 0,
    _terms = Map.empty,
    _rewriteState = NonNormal
  }
  extractMultipliers seq' = Map.foldlWithKey addScaledTerm base (_terms seq')
    where
    base = (empty :: PairSeq SumTag e) { _overall = _overall seq' }
    addScaledTerm :: PairSeq SumTag e -> Expression e -> Rational -> PairSeq SumTag e
    addScaledTerm ps expr coeff = addTerm ps expr' $ coeff * multiplier
      where
      (multiplier, expr') = extractMultiplier expr
  wrap = Sum
  addTerm seq' expr coeff = newSeq
    where
    newSeq = case expr of
      ConstantRational r -> seq' { _overall = (_overall seq') + (r * coeff) }
      Sum subSeq -> Map.foldlWithKey addTerm' incorporateOverall (_terms subSeq)
       where
       incorporateOverall = addTerm seq' (ConstantRational $ _overall subSeq) coeff
       addTerm' s e c = addTerm s e $ c * coeff
      _ -> normalInsert
      where
      normalInsert = seq' { _terms = Map.insertWith (+) expr coeff (_terms seq') }

instance PairSeqLike ProdTag e where
  empty = PairSeq {
    _overall = 1,
    _terms = Map.empty,
    _rewriteState = NonNormal
  }
  extractMultipliers seq' = Map.foldlWithKey addScaledTerm base (_terms seq')
    where
    base = (empty :: PairSeq ProdTag e) { _overall = _overall seq' }
    addScaledTerm :: PairSeq ProdTag e -> Expression e -> Rational -> PairSeq ProdTag e
    addScaledTerm ps expr coeff = newSeq
      where
      (multiplier, expr') = extractMultiplier expr
      isIntegerCoeff = denominator coeff == 1
      newSeq = if isIntegerCoeff
        then addTerm (addTerm ps (ConstantRational multiplier) coeff) expr' coeff
        else addTerm ps expr coeff
  wrap = Product
  addTerm seq' expr coeff = newSeq
    where
    isIntegerCoeff = denominator coeff == 1
    newSeq = if isIntegerCoeff
      then case expr of
        ConstantRational r -> seq' { _overall = (_overall seq') * (r ^^ numerator coeff) }
        Product subSeq -> Map.foldlWithKey addTerm' incorporateOverall (_terms subSeq)
         where
         incorporateOverall = addTerm seq' (ConstantRational $ _overall subSeq) coeff
         addTerm' s e c = addTerm s e $ c * coeff
        _ -> normalInsert
      else normalInsert
      where
      normalInsert = seq' { _terms = Map.insertWith (+) expr coeff (_terms seq') }

removeZeros :: PairSeq t e -> PairSeq t e
removeZeros seq' = seq' { _terms = terms' }
  where
  terms' = Map.filter (/= 0) $ _terms seq'

Lens.makeLenses ''PairSeq

add :: Ord e => Expression e -> Expression e -> Expression e
add a b = Sum $ PairSeq {
  _overall = 0,
  _terms = Map.fromList [(a, 1), (b, 1)],
  _rewriteState = NonNormal
}

sub :: Ord e => Expression e -> Expression e -> Expression e
sub a b = Sum $ PairSeq {
  _overall = 0,
  _terms = Map.fromList [(a, 1), (b, -1)],
  _rewriteState = NonNormal
}

mul :: Ord e => Expression e -> Expression e -> Expression e
mul a b = Product $ PairSeq {
  _overall = 1,
  _terms = Map.fromList [(a, 1), (b, 1)],
  _rewriteState = NonNormal
}

hasNullOverall :: forall t e . PairSeqLike t e => PairSeq t e -> Bool
hasNullOverall seq' = (_overall seq') == (_overall (empty :: PairSeq t e))

simplify :: Ord e => Expression e -> Expression e
simplify (Product seq') = simplifyPairSeq seq';
simplify (Sum seq') = simplifyPairSeq seq';
simplify a = a

simplifyPairSeq :: (PairSeqLike t e, Ord e) => PairSeq t e -> Expression e
simplifyPairSeq seq' = newExpr
  where
  normalised = normalise seq'
  newExpr = case Map.toList (_terms normalised) of
    [] -> ConstantRational $ _overall normalised
    [(a, 1)] -> if (hasNullOverall normalised)
      then a
      else wrap normalised
    _ -> wrap normalised

instance Ord e => Num (Expression e) where
  fromInteger a = ConstantRational (a % 1)
  (+) a b = simplify $ add a b
  (-) a b = simplify $ sub a b
  (*) a b = simplify $ mul a b
  abs = simplify . Abs
  signum = simplify . Signum
