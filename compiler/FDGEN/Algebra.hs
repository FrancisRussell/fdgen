{-# LANGUAGE TemplateHaskell, EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
module FDGEN.Algebra ( Expression(..), subst, substSymbols, lagrange, diff, integrate, expand
                     , definiteIntegrate, adamsBashforth, adamsBashforthGeneral, vars
                     , polyCoeff, polyCoeffs, expandSymbols, rewrite, rewriteFixedPoint
                     , PairSeq(..)) where
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.List (genericIndex)
import Data.Ratio ((%), denominator, numerator)
import Data.Foldable (foldl')
import FDGEN.Pretty (PrettyPrintable(..))
import FDGEN.Precedence (Assoc(..), Precedence(..), pDoc, PDoc(..), renderTerminal, renderInfix)
import Text.PrettyPrint (hcat, char, parens, punctuate)
import Control.Applicative ((<$>))
import Control.Monad (liftM2)
import qualified Data.Map.Strict as Map
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
  | Diff (Expression e) e Integer
  | Int (Expression e) e
  | Function e [Expression e]
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
  empty = PairSeq
    {  _psOverall = 0
    ,  _psTerms = Map.empty
    ,  _psRewriteState = NonNormal
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
  empty = PairSeq
    { _psOverall = 1
    , _psTerms = Map.empty
    , _psRewriteState = NonNormal
    }
  extractMultipliers seq' = if _psOverall rebuilt == 0
    then Lens.set psOverall 0 (empty :: PairSeq ProdTag e)
    else rebuilt
    where
    rebuilt = Map.foldlWithKey addScaledTerm base $ _psTerms seq'
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
    normalInsert = incrementTerm expr coeff seq'
    newSeq = if isIntegerCoeff
      then case expr of
        ConstantRational r -> Lens.over psOverall (* (r ^^ numerator coeff)) seq'
        Product subSeq -> foldl' addTerm' seq' $ toPairs subSeq
          where
          addTerm' s (e, c) = addTerm e (c * coeff) s
        _ -> normalInsert
      else normalInsert

incrementTerm :: Ord e => Expression e -> Rational -> PairSeq t e -> PairSeq t e
incrementTerm expr coeff = Lens.over psTerms $ Map.insertWith (+) expr coeff

toPairs :: PairSeqLike t e => PairSeq t e -> [(Expression e, Rational)]
toPairs seq' = if hasNullOverall seq'
  then terms
  else overall:terms
  where
  overall = (ConstantRational $ _psOverall seq', 1)
  terms = Map.assocs $ _psTerms seq'

fromPairs :: (Ord e, PairSeqLike t e) => [(Expression e, Rational)] -> PairSeq t e
fromPairs = foldl' (flip $ uncurry addTerm) empty

removeZeros :: PairSeq t e -> PairSeq t e
removeZeros = Lens.over psTerms $ Map.filter (/= 0)

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
hasNullOverall seq' = _psOverall seq' == _psOverall (empty :: PairSeq t e)

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
simplify (Product seq') = simplify' $ simplifyPairSeq seq'
simplify (Sum seq') = simplify' $ simplifyPairSeq seq'
simplify (Abs e) = simplify' . Abs $ simplify e
simplify (Signum e) = simplify' . Signum $ simplify e
simplify e = simplify' e

simplify' :: Ord e => Expression e -> Expression e
simplify' (Abs (ConstantRational r)) = ConstantRational $ abs r
simplify' (Abs (ConstantFloat f)) = ConstantFloat $ abs f
simplify' (Signum (ConstantRational r)) = ConstantRational $ signum r
simplify' (Signum (ConstantFloat f)) = ConstantFloat $ signum f
simplify' (Diff e sym i) = genericIndex (iterate (diff' sym) e) i
simplify' (Int e sym) = integrate' sym e
simplify' expr@(Product seq') = case _psOverall seq' of
  0 -> 0
  _ -> expr
simplify' e = e

rewrite :: Ord e => (Expression e -> Expression e) -> Expression e -> Expression e
rewrite f = simplify' . f . rewrite' f

rewritePairSeq :: (Ord e, PairSeqLike t e) => (Expression e -> Expression e) -> PairSeq t e -> PairSeq t e
rewritePairSeq f = foldl' addTerm' empty . toPairs
  where
  addTerm' s (e, r) = addTerm (rewrite f e) r s

rewrite' :: Ord e => (Expression e -> Expression e) -> Expression e -> Expression e
rewrite' f (Sum seq') = simplifyPairSeq $ rewritePairSeq f seq'
rewrite' f (Product seq') = simplifyPairSeq $ rewritePairSeq f seq'
rewrite' f (Abs e) = Abs . simplify' $ rewrite f e
rewrite' f (Signum e) = Signum . simplify' $ rewrite f e
rewrite' f (Ln e) = Ln . simplify' $ rewrite f e
rewrite' f (Diff e sym n) = Diff (simplify' $ rewrite f e) (rewriteSymbol f sym) n
rewrite' f (Int e sym) = Int (simplify' $ rewrite f e) (rewriteSymbol f sym)
rewrite' f (Function sym params) = Function (rewriteSymbol f sym) $ (simplify' . rewrite f) <$> params
rewrite' _ e@(Symbol _) = e
rewrite' _ e@(ConstantFloat _) = e
rewrite' _ e@(ConstantRational _) = e

rewriteSymbol :: (Expression e -> Expression f) -> e -> f
rewriteSymbol f sym = case f (Symbol sym) of
  Symbol s -> s
  _ -> error "rewriteSymbol: cannot subsitute variable with complex expression"

subst :: Ord e => Expression e -> Expression e -> Expression e -> Expression e
subst from to = simplify . rewrite update
  where
  update e' = if e' == from then to else e'

substSymbols :: (Ord e, Ord f) => (e -> f) -> Expression e -> Expression f
substSymbols f = expandSymbols (Symbol . f)

expandSymbols :: (Ord e, Ord f) => (e -> Expression f) -> Expression e -> Expression f
expandSymbols f expr = simplify $ case expr of
  Symbol s -> f s
  ConstantFloat r -> ConstantFloat r
  ConstantRational r -> ConstantRational r
  Abs e -> Abs $ expandSymbols f e
  Ln e -> Ln $ expandSymbols f e
  Signum e -> Signum $ expandSymbols f e
  Sum seq' -> Sum . fromPairs $ transformPair <$> toPairs seq'
  Product seq' -> Product . fromPairs $ transformPair <$> toPairs seq'
  Diff e sym i -> Diff (expandSymbols f e) (replaceSymbol sym) i
  Int e sym -> Int (expandSymbols f e) (replaceSymbol sym)
  Function sym params -> Function (replaceSymbol sym) (expandSymbols f <$> params)
  where
  transformPair (e, r) = (expandSymbols f e, r)
  replaceSymbol sym = case f sym of
    Symbol s -> s
    _ -> error "expandSymbols: cannot substitute variable with complex expression"

lagrange :: Ord e => Expression e -> [(Expression e, Expression e)] -> Expression e
lagrange sym points = foldl' (+) 0 bases
  where
  bases = constructBasis <$> [0 .. (length points - 1)]
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
depends syms (Diff e _sym _) = depends syms e
depends syms (Int e sym) = Set.member sym syms || depends syms e
depends syms (Function sym params) =
  foldl (||) (Set.member sym syms) (depends syms <$> params)

vars :: Ord e => Expression e -> Set e
vars (Symbol s) = Set.singleton s
vars (Sum seq') =
  foldl (Set.union) Set.empty $ map (\(a, _) -> vars a) (toPairs seq')
vars (Product seq') =
  foldl (Set.union) Set.empty $ map (\(a, _) -> vars a) (toPairs seq')
vars (ConstantFloat _) = Set.empty
vars (ConstantRational _) = Set.empty
vars (Abs expr) = vars expr
vars (Signum expr) = vars expr
vars (Ln expr) = vars expr
vars (Diff e _sym _) = vars e
vars (Int e sym) = Set.insert sym (vars e)
vars (Function sym params) = foldl Set.union (Set.singleton sym) (vars <$> params)

polyCoeff :: Ord e => Expression e -> e -> Int -> Maybe (Expression e)
polyCoeff expr sym power = (!! power) <$> (++ repeat 0) <$> polyCoeffs expr sym

polyCoeffs :: Ord e => Expression e -> e -> Maybe [Expression e]
polyCoeffs expr sym = if depends (Set.singleton sym) expr
  then polyCoeffs' expr
  else Just [expr]
  where
  combineCoeffs :: Ord e => [Expression e] -> [Expression e] -> [Expression e]
  combineCoeffs a b = take totalLength $ zipWith (+) a' b'
    where
    a' = a ++ (repeat 0)
    b' = b ++ (repeat 0)
    totalLength = max (length a) (length b)
  raiseCoeffs :: Ord e => [Expression e] -> Rational -> Maybe [Expression e]
  raiseCoeffs e p = if denominator p /= 1 || numerator p < 0
    then Nothing
    else case numerator p of
    0 -> Just [1.0]
    1 -> Just e
    num -> if num `mod` 2 == 1
      then multiplyCoeffs e <$> raisedEven
      else raisedEven
      where
      raisedEven = (liftM2 multiplyCoeffs) half half
      half = raiseCoeffs e (fromInteger $ num `div` 2)
  multiplyCoeffs :: Ord e => [Expression e] -> [Expression e] -> [Expression e]
  multiplyCoeffs a b = [foldl (+) 0 (subTerms idx)| idx <- [0..totalLength-1]]
    where
    totalLength = (max 1 (length a)) + (max 1 (length b)) - 1
    pad l = l ++ (take (totalLength - length l) $ repeat 0)
    a' = pad a
    b' = pad b
    subTerms idx = zipWith (*) a' (drop (totalLength - idx - 1) (reverse b'))
  polyCoeffs' (Symbol s) = Just $ if s == sym then [0.0, 1.0] else []
  polyCoeffs' (Sum seq') =
    foldl (liftM2 combineCoeffs) (Just []) $ map (\(e, c) -> polyCoeffs (fromRational c * e) sym) (toPairs seq')
  polyCoeffs' (Product seq') =
    foldl (liftM2 multiplyCoeffs) (Just [1.0]) $ map (\(a, p) -> (flip raiseCoeffs $ p) =<< (polyCoeffs a sym)) (toPairs seq')
  polyCoeffs' n@(ConstantFloat _) = Just [n]
  polyCoeffs' n@(ConstantRational _) = Just [n]
  polyCoeffs' (Abs _expr) = Nothing
  polyCoeffs' (Signum _expr) = Nothing
  polyCoeffs' (Ln _expr) = Nothing
  polyCoeffs' (Diff _expr _sym _power) = Nothing
  polyCoeffs' (Int _expr _sym) = Nothing
  polyCoeffs' (Function _sym _params) = Nothing

extractElem :: [a] -> Int -> (a, [a])
extractElem lst index = (elem', rest)
  where
  (header, elem' : footer) = splitAt index lst
  rest = header ++ footer

diff :: Ord e => e -> Expression e -> Expression e
diff sym = simplify . diff' sym

diff' :: Ord e => e -> Expression e -> Expression e
diff' sym expression = case expression of
  ConstantFloat _ -> 0
  ConstantRational _ -> 0
  Symbol s -> if s == sym then 1 else 0
  Abs x -> Signum x * diff sym x
  Signum _ -> 0
  Ln x -> raise x (-1) * diff sym x
  Sum seq' -> Sum $ fromPairs differentiatedPairs
    where
    differentiatedPairs = (\(a,b) -> (diff sym a, b)) <$> toPairs seq'
  Product seq' -> Sum $ fromPairs subTerms
    where
    pairs = toPairs seq'
    subTerms = (constructSubTerm . extractElem pairs) <$> [0 .. length pairs - 1]
    constructSubTerm ((expr, power), rest) = (Product $ fromPairs newTerms, power)
      where
      newTerms = (diff sym expr, 1):(expr, power - 1):rest
  Diff e s i -> if sym == s
    then Diff e s (i+1)
    else diffNoun
  Int e s -> if sym == s
    then e
    else diffNoun
  Function _ _ -> diffNoun
  where
  diffNoun = if depends (Set.singleton sym) expression
    then Diff expression sym 1
    else 0

splitPairSeqDepends :: (PairSeqLike t e, Ord e) => Set e -> PairSeq t e -> ([(Expression e, Rational)], [(Expression e, Rational)])
splitPairSeqDepends syms seq' = foldl' combine ([], []) $ toPairs seq'
  where
  combine (dep, nodep) (expr, coeff) = if depends syms expr
    then (addTerm' dep, nodep)
    else (dep, addTerm' nodep)
    where
    addTerm' terms = (expr, coeff):terms

definiteIntegrate :: Ord e => e -> Expression e -> Expression e -> Expression e -> Expression e
definiteIntegrate sym low high =
  differenceAtLimits sym low high . integrate sym

differenceAtLimits :: Ord e => e -> Expression e -> Expression e -> Expression e -> Expression e
differenceAtLimits sym low high expr =
  expand $ (exprAt high) - (exprAt low)
  where
  exprAt pos = subst (Symbol sym) pos expr

integrate :: Ord e => e -> Expression e -> Expression e
integrate sym = simplify . integrate' sym . expand

integrate' :: Ord e => e -> Expression e -> Expression e
integrate' sym expr = case expr of
  Abs _ -> integrateByParts sym expr 1
  Signum _ -> integrateByParts sym expr 1
  Ln _ -> integrateByParts sym expr 1
  ConstantFloat _ -> Symbol sym * expr
  ConstantRational _ -> Symbol sym * expr
  Symbol s -> if s == sym
    then (Symbol s ^ (2 :: Integer)) * (fromRational $ 1 % 2)
    else Symbol s * Symbol sym
  Sum seq' -> Sum $ fromPairs integratedPairs
    where
    integratedPairs = map (\(a, b) -> (integrate sym a, b)) $ toPairs seq'
  Product seq' -> integratedDep * (Product $ fromPairs indep)
    where
    (dep, indep) = splitPairSeqDepends (Set.singleton sym) seq'
    integratedDep = integrateProductSeq sym dep
  Diff _ _ _ -> intNoun
  Int _ _ -> intNoun
  Function _ _ -> intNoun
  where
  intNoun = if depends (Set.singleton sym) expr
    then Int expr sym
    else Symbol sym * expr

integrateProductSeq :: Ord e => e -> [(Expression e, Rational)] -> Expression e
integrateProductSeq sym [] = Symbol sym
integrateProductSeq sym [(expr, exp')]
  | exp' == 0       = symExpr
  | expr == symExpr = if exp' /= -1
                        then raise symExpr (exp' + 1) / fromRational (exp' + 1)
                        else Ln symExpr
  | exp' == 1       = integrate sym expr
  | exp' > 1 && denominator exp' == 1 = integrateProductSeq sym [(expr, exp1 % 1), (expr, exp2 % 1)]
  | otherwise = error "Cannot integrate negative or fractional exponents of complex expression"
  where
  symExpr = Symbol sym
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
integrateByParts sym toDiff toInt = if (u == 0)
  then 0
  else u * iv + remainder
  where
  u = toDiff
  v = toInt
  iv = integrate sym v
  du = diff sym u
  remainder = integrate sym $ du*iv*(-1)

rewriteFixedPoint :: Ord e => (Expression e -> Expression e) -> Expression e -> Expression e
rewriteFixedPoint f expr = fixed rewrites
  where
  pass = simplify . rewrite f
  rewrites = iterate pass expr
  fixed (x1:x2:xs) = if x1 == x2 then x1 else fixed (x2:xs)
  fixed _ = error "fixed should be applied to infinite list"

-- Fails to expand: (a / b + c) * (1 / (h + i))
-- This is due to multiplication of sums raised to negative powers.
-- Using iterate (by using rewriteFixedPoint) is a hack and might cause infinite iteration if
-- the expanded form is non-unique.
expand :: Ord e => Expression e -> Expression e
expand = rewriteFixedPoint expand'

expand' :: Ord e => Expression e -> Expression e
expand' (Product seq') = constructExpandedProduct $ toPairs seq'
expand' x = x

mulSum :: Ord e => PairSeq SumTag e -> Expression e -> PairSeq SumTag e
mulSum seq' (Sum seq'') = fromPairs result
  where
  result = do
    (e1, c1) <- toPairs seq'
    (e2, c2) <- toPairs seq''
    return (e1 * e2, c1 * c2)
mulSum seq' expr = fromPairs $ (\(a,b) -> (a * expr, b)) <$> toPairs seq'

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

data TempSymbol e
  = Original e
  | Temporary String
  deriving (Eq, Ord, Show)

toTempSymbol :: e -> TempSymbol e
toTempSymbol s = Original s

fromTempSymbol :: TempSymbol e -> e
fromTempSymbol (Original s) = s
fromTempSymbol (Temporary s) = error $ "fromTempSymbol: unexpected symbol " ++ s ++ " in expression"

adamsBashforth :: Ord e => e -> Expression e -> [Expression e] -> Expression e
adamsBashforth = adamsBashforthGeneral 1

adamsBashforthGeneral :: Ord e => Integer -> e -> Expression e -> [Expression e] -> Expression e
adamsBashforthGeneral numInt h y0 fs = if numInt < 0
 then error "adamsBashforthGeneral: number of integrals must be >=0"
 else y0 + substSymbols fromTempSymbol defInt
  where
  hSym = Symbol $ Original h
  var = Temporary "x"
  genPoints _ [] = []
  genPoints hCoeff (d:ds) = (hCoeff * hSym, d):(genPoints (hCoeff-1) ds)
  points = genPoints (-1) $ substSymbols toTempSymbol <$> fs
  interpolatedDerivatives = lagrange (Symbol var) points
  defInt = differenceAtLimits var (-hSym) 0 indefInt
  indefInt = genericIndex (iterate (integrate var) interpolatedDerivatives) numInt

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

mulPrec = PrecLevel 2
addPrec = PrecLevel 1
powPrec = PrecLevel 0

instance PrettyPrintable e => PrettyPrintable (Expression e) where
  toDoc expr = pDoc $ toPDoc expr
    where
    renderInteger :: Integer -> PDoc
    renderInteger i = if i >= 0
      then PDoc (toDoc i) NoAssoc AtomPrec
      else PDoc (hcat [char '-', toDoc (-i)]) NoAssoc addPrec
    renderRational r = case denominator r of
       1 -> renderInteger $ numerator r
       _ -> renderDivision (renderInteger $ numerator r) (renderInteger $ denominator r)
    renderDivision = renderInfix ("/", mulPrec, LeftAssoc)
    renderMultiplication = renderInfix ("*", mulPrec, LeftAssoc)
    renderAddition = renderInfix ("+", addPrec, LeftAssoc)
    renderPower = renderInfix ("^", powPrec, RightAssoc)
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
      Diff e s i -> renderFunction "diff" $ [toPDoc e, renderTerminal s] ++ if i == 1
        then []
        else [renderTerminal i]
      Int e s -> renderFunction "int" [toPDoc e, renderTerminal s]
      Function sym params -> renderFunction (prettyPrint sym) (toPDoc <$> params)
