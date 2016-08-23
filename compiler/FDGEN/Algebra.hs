{-# LANGUAGE TemplateHaskell, EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
module FDGEN.Algebra (Expression(..)) where
import Data.Map (Map)
import Data.Ratio ((%), denominator, numerator)
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
       gcd' r1 r2 = (gcd (numerator r1) (numerator r2)) % (gcd (denominator r1) (denominator r2))
       common = foldl gcd' 1 (overall:(Map.elems terms))
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
  addTerm expr coeff seq' = newSeq
    where
    newSeq = case expr of
      ConstantRational r -> Lens.over psOverall (+ (r * coeff)) seq'
      Sum subSeq -> Map.foldlWithKey addTerm' incorporateOverall (_psTerms subSeq)
       where
       incorporateOverall = addTerm (ConstantRational $ _psOverall subSeq) coeff seq'
       addTerm' s e c = addTerm e (c * coeff) s
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
        then foldl (flip $ uncurry addTerm) ps [(ConstantRational multiplier, coeff), (expr', coeff)]
        else addTerm expr coeff ps
  wrap = Product
  addTerm expr coeff seq' = newSeq
    where
    isIntegerCoeff = denominator coeff == 1
    newSeq = if isIntegerCoeff
      then case expr of
        ConstantRational r -> Lens.over psOverall (* (r ^^ numerator coeff)) seq'
        Product subSeq -> Map.foldlWithKey addTerm' incorporateOverall (_psTerms subSeq)
         where
         incorporateOverall = addTerm (ConstantRational $ _psOverall subSeq) coeff seq'
         addTerm' s e c = addTerm e (c * coeff) s
        _ -> normalInsert
      else normalInsert
      where
      normalInsert = incrementTerm expr coeff seq'

incrementTerm :: Ord e => Expression e -> Rational -> PairSeq t e -> PairSeq t e
incrementTerm expr coeff = Lens.over psTerms (Map.insertWith (+) expr coeff)

removeZeros :: PairSeq t e -> PairSeq t e
removeZeros seq' = seq' { _psTerms = terms' }
  where
  terms' = Map.filter (/= 0) $ _psTerms seq'

add :: Ord e => Expression e -> Expression e -> Expression e
add a b = Sum $ foldr (uncurry addTerm) empty [(a, 1), (b, 1)]

sub :: Ord e => Expression e -> Expression e -> Expression e
sub a b = Sum $ foldr (uncurry addTerm) empty [(a, 1), (b, -1)]

mul :: Ord e => Expression e -> Expression e -> Expression e
mul a b = Product $ foldr (uncurry addTerm) empty [(a, 1), (b, 1)]

divide :: Ord e => Expression e -> Expression e -> Expression e
divide a b = Sum $ foldr (uncurry addTerm) empty [(a, 1), (b, -1)]

hasNullOverall :: forall t e . PairSeqLike t e => PairSeq t e -> Bool
hasNullOverall seq' = (_psOverall seq') == (_psOverall (empty :: PairSeq t e))

simplify :: Ord e => Expression e -> Expression e
simplify (Product seq') = simplifyPairSeq seq'
simplify (Sum seq') = simplifyPairSeq seq'
simplify (Abs (ConstantRational r)) = ConstantRational $ abs r
simplify (Signum (ConstantRational r)) = ConstantRational $ signum r
simplify (Abs (ConstantFloat f)) = ConstantFloat $ abs f
simplify (Signum (ConstantFloat f)) = ConstantFloat $ signum f
simplify a = a

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
