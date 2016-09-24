{-# LANGUAGE TemplateHaskell, EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
module FDGEN.Algebra (Expression(..), subst, lagrange, diff, integrate, expand) where
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Ratio ((%), denominator, numerator)
import Data.Foldable (foldl')
import FDGEN.Pretty (PrettyPrintable(..))
import Text.PrettyPrint (Doc, hcat, char, parens, punctuate)
import Control.Applicative ((<$>))
import qualified Data.Map as Map
import qualified Data.Set as Set
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
  | Ln (Expression e)
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
         (_, 0) -> abs r1
         (0, _) -> abs r2
         (_, _) -> (gcd (numerator r1) (numerator r2)) % (gcd (denominator r1) (denominator r2))
       coeffs = filter (/= 0) $ overall:(Map.elems terms)
       moreNegative = (length $ filter (< 0) coeffs) > div (length coeffs) 2
       sign = if moreNegative then (-1) else 1
       factor = sign * foldl' gcd' 0 coeffs
       common = if factor == 0 then 1 else factor
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
    base = Lens.set psOverall (_psOverall seq') (empty :: PairSeq SumTag e)
    addScaledTerm :: PairSeq SumTag e -> Expression e -> Rational -> PairSeq SumTag e
    addScaledTerm ps expr coeff = addTerm expr' (coeff * multiplier) ps
      where
      (multiplier, expr') = extractMultiplier expr
  wrap = Sum
  addTerm expr coeff seq' = case expr of
      ConstantRational r -> Lens.over psOverall (+ (r * coeff)) seq'
      Sum subSeq -> foldl' addTerm' seq' (toPairs subSeq)
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
  extractMultipliers seq' = if _psOverall rebuilt == 0
    then Lens.set psOverall 0 (empty :: PairSeq ProdTag e)
    else rebuilt
    where
    rebuilt = Map.foldlWithKey addScaledTerm base (_psTerms seq')
    base = Lens.set psOverall (_psOverall seq') (empty :: PairSeq ProdTag e)
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
        Product subSeq -> foldl' addTerm' seq' (toPairs subSeq)
         where
         addTerm' s (e, c) = addTerm e (c * coeff) s
        _ -> normalInsert
      else normalInsert
      where
      normalInsert = incrementTerm expr coeff seq'

incrementTerm :: Ord e => Expression e -> Rational -> PairSeq t e -> PairSeq t e
incrementTerm expr coeff = Lens.over psTerms (Map.insertWith (+) expr coeff)

toPairs :: PairSeqLike t e => PairSeq t e -> [(Expression e, Rational)]
toPairs seq' = if hasNullOverall seq'
  then terms
  else overall:terms
  where
  overall = (ConstantRational $ _psOverall seq', 1)
  terms = Map.assocs $ _psTerms seq'

fromPairs :: (Ord e, PairSeqLike t e) => [(Expression e, Rational)] -> PairSeq t e
fromPairs pairs = foldl' (flip $ uncurry addTerm) empty pairs

removeZeros :: PairSeq t e -> PairSeq t e
removeZeros seq' = seq' { _psTerms = terms' }
  where
  terms' = Map.filter (/= 0) $ _psTerms seq'

add :: Ord e => Expression e -> Expression e -> Expression e
add a b = Sum $ fromPairs [(a, 1), (b, 1)]

sub :: Ord e => Expression e -> Expression e -> Expression e
sub a b = Sum $ fromPairs [(a, 1), (b, -1)]

mul :: Ord e => Expression e -> Expression e -> Expression e
mul a b = Product $ fromPairs [(a, 1), (b, 1)]

divide :: Ord e => Expression e -> Expression e -> Expression e
divide a b = Product $ fromPairs [(a, 1), (b, -1)]

raise :: Ord e => Expression e -> Rational -> Expression e
raise a b = Product $ fromPairs [(a, b)]

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
rewritePairSeq f seq' = foldl' addTerm' empty $ toPairs seq'
  where
  addTerm' s (e, r) = addTerm (rewrite f e) r s

rewrite' :: Ord e => (Expression e -> Expression e) -> Expression e -> Expression e
rewrite' f (Sum seq') = simplifyPairSeq $ rewritePairSeq f seq'
rewrite' f (Product seq') = simplifyPairSeq $ rewritePairSeq f seq'
rewrite' f (Abs e) = Abs . simplify' $ rewrite f e
rewrite' f (Signum e) = Signum . simplify' $ rewrite f e
rewrite' f (Ln e) = Ln . simplify' $ rewrite f e
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

depends :: Ord e => Set e -> Expression e -> Bool
depends syms (Symbol s) = Set.member s syms
depends syms (Sum seq') =
  foldl (||) False $ map (\(a, _) -> depends syms a) $ toPairs seq'
depends syms (Product seq') =
  foldl (||) False $ map (\(a, _) -> depends syms a) $ toPairs seq'
depends _ (ConstantFloat _) = False
depends _ (ConstantRational _) = False
depends syms (Abs expr) = depends syms expr
depends syms (Signum expr) = depends syms expr
depends syms (Ln expr) = depends syms expr

extractElem :: [a] -> Int -> (a, [a])
extractElem lst index = (elem', rest)
  where
  (header, elem' : footer) = splitAt index lst
  rest = header ++ footer

diff :: Ord e => e -> Expression e -> Expression e
diff sym = simplify . diff' sym

diff' :: Ord e => e -> Expression e -> Expression e
diff' _ (ConstantFloat _) = 0
diff' _ (ConstantRational _) = 0
diff' sym (Symbol s) = if s == sym then 1 else 0
diff' sym (Abs x) = (Signum x) * diff sym x
diff' _ (Signum _) = 0
diff' sym (Ln x) = raise x (-1) * diff sym x
diff' sym (Sum seq') = Sum $ fromPairs differentiatedPairs
  where
  differentiatedPairs = map (\(a,b) -> (diff sym a, b)) $ toPairs seq'
diff' sym (Product seq') = Sum $ fromPairs subTerms
  where
  pairs = toPairs seq'
  subTerms = map (constructSubTerm . extractElem pairs) [0 .. length pairs - 1]
  constructSubTerm ((expr, power), rest) = (Product $ fromPairs newTerms, power)
    where
    newTerms = (diff sym expr, 1):(expr, power - 1):rest

splitPairSeqDepends :: (PairSeqLike t e, Ord e) => Set e -> PairSeq t e -> ([(Expression e, Rational)], [(Expression e, Rational)])
splitPairSeqDepends syms seq' = foldl' combine ([], []) $ toPairs seq'
  where
  combine (dep, nodep) (expr, coeff) = if depends syms expr
    then (addTerm' dep, nodep)
    else (dep, addTerm' nodep)
      where
      addTerm' terms = (expr, coeff):terms

definiteIntegrate :: (Ord e, Num n) => e -> Expression e -> Expression e -> Expression e -> Expression e
definiteIntegrate sym low high expr = expand $ highInt - lowInt
  where
  integrated = integrate sym expr
  lowInt = subst (Symbol sym) low integrated
  highInt = subst (Symbol sym) high integrated

integrate :: Ord e => e -> Expression e -> Expression e
integrate sym = simplify . integrate' sym

integrate' :: Ord e => e -> Expression e -> Expression e
integrate' sym expr@(Abs _) = integrateByParts sym expr 1
integrate' sym expr@(Signum _) = integrateByParts sym expr 1
integrate' sym expr@(Ln _) = integrateByParts sym expr 1
integrate' sym c@(ConstantFloat _) = Symbol sym * c
integrate' sym c@(ConstantRational _) = Symbol sym * c
integrate' sym (Symbol s) = if s == sym
  then ((Symbol s) ^ (2 :: Integer)) * (fromRational $ 1 % 2)
  else (Symbol s) * (Symbol sym)
integrate' sym (Sum seq') = Sum $ fromPairs integratedPairs
  where
  integratedPairs = map (\(a, b) -> (integrate sym a, b)) $ toPairs seq'
integrate' sym (Product seq') = integratedDep * (Product $ fromPairs indep)
  where
  (dep, indep) = splitPairSeqDepends (Set.singleton sym) seq'
  integratedDep = integrateProductSeq sym dep

integrateProductSeq :: Ord e => e -> [(Expression e, Rational)] -> Expression e
integrateProductSeq sym [] = Symbol sym
integrateProductSeq sym [(expr, exp')] = if exp' == 0
  then Symbol sym
  else if expr == Symbol sym
  then if exp' /= -1
    then (raise (Symbol sym) (exp' + 1)) / fromRational (exp' + 1)
    else Ln $ Symbol sym
  else if exp' == 1
  then integrate sym expr
  else if exp' > 1 && denominator exp' == 1
  then integrateProductSeq sym [(expr, exp1 % 1), (expr, exp2 % 1)]
  else error "Cannot integrate negative or fractional exponents of complex expression"
    where
    iexp = numerator exp'
    exp1 = iexp `div` 2
    exp2 = iexp - exp1
integrateProductSeq sym [(expr1, 1), (expr2, 1)] = integrateByParts sym expr1 expr2
integrateProductSeq sym pairs = integrateByParts sym prod1 prod2
  where
  (pairs1, pairs2) = splitAt (length pairs `div` 2) pairs
  prod1 = Product $ fromPairs pairs1
  prod2 = Product $ fromPairs pairs2

integrateByParts :: Ord e => e -> Expression e -> Expression e -> Expression e
integrateByParts sym toDiff toInt =
  if (u == 0)
  then 0
  else u * iv + remainder
  where
  u = toDiff
  v = toInt
  iv = integrate sym v
  du = diff sym u
  remainder = integrate sym $ du*iv*(-1)

--trivialIntegrate :: Ord e => String -> e -> Expression e -> Expression e
--trivialIntegrate op sym expr = if depends (Set.singleton sym) expr
--  then error $ "Do not know how to integrate " ++ op
--  else Symbol sym * expr


-- Fails to expand: (a / b + c) * (1 / (h + i))
-- This is due to multiplication of sums raised to negative powers.
-- Using iterate is a hack and might cause infinite iteration if
-- the expanded form is non-unique.
expand :: Ord e => Expression e -> Expression e
expand expr = fixed rewrites
  where
  pass = simplify . rewrite expand'
  rewrites = iterate pass expr
  fixed (x1:x2:xs) = if x1 == x2 then x1 else fixed (x2:xs)
  fixed _ = error "fixed should be applied to infinite list"

expand' :: forall e . Ord e => Expression e -> Expression e
expand' (Product seq') = constructExpandedProduct $ toPairs seq'
expand' x = x

mulSum :: Ord e => PairSeq SumTag e -> Expression e -> PairSeq SumTag e
mulSum seq' (Sum seq'') = fromPairs result
  where
  result = do
    (e1, c1) <- toPairs seq'
    (e2, c2) <- toPairs seq''
    return (e1 * e2, c1 * c2)
mulSum seq' expr = fromPairs . map (\(a,b) -> (a * expr, b)) $ toPairs seq'

constructExpandedProduct :: forall e . Ord e => [(Expression e, Rational)] ->
                            Expression e
constructExpandedProduct seq' = Sum $ mulSum numeratorTerm denominatorTerm
  where
  unitSum = (fromPairs [(1,1)] :: PairSeq SumTag e)
  q = (unitSum, unitSum)
  q' = foldl' mulTerm q seq'
  numeratorTerm = fst q'
  denominatorTerm = Product $ fromPairs [(Sum $ snd q', -1)]
  mulTerm :: (PairSeq SumTag e, PairSeq SumTag e) -> (Expression e, Rational) ->
             (PairSeq SumTag e, PairSeq SumTag e)
  mulTerm (qn, qd) (expr, exp') = if numerator exp' >= 0
    then (mulSum qn raised, qd)
    else (qn, mulSum qd raised)
    where
    absExp = abs exp'
    raised = if denominator exp' /= 1
      then raise expr absExp
      else raiseInt (numerator absExp)
    raiseInt :: Integer -> Expression e
    raiseInt 0 = 1
    raiseInt 1 = expr
    raiseInt n = if n < 0
      then error "Cannot raise to negative exponent"
      else constructExpandedProduct prodTerms
      where
        even' = constructExpandedProduct [(expr, fromInteger $ n `div` 2)]
        prodTerms = if n `mod` 2 == 0
          then [(even', 1), (even', 1)]
          else [(expr, fromInteger $ n - 1), (expr, 1)]

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
      Signum e -> renderFunction "sgn" [toPDoc e]
      Ln e -> renderFunction "ln" [toPDoc e]
