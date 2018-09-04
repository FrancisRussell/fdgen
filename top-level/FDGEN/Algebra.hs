{-# LANGUAGE TemplateHaskell, EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
module FDGEN.Algebra ( Expression(..), subst, substSymbols, lagrange, diff, integrate, expand
                     , definiteIntegrate, adamsBashforth, adamsBashforthGeneral, vars
                     , polyCoeff, polyCoeffs, expandSymbols, rewrite, rewriteFixedPoint
                     , PairSeq(..), Special(..), fromSpecial, UnaryFunction(..), BinaryFunction(..)
                     , FunctionApplication(..)) where
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

-- | Special constants
data Special
  = Euler
  | Pi
  deriving (Eq, Ord, Show)

-- | Built in unary functions
data UnaryFunction
  = Cos
  | Sin
  | Tan
  | Abs
  | Signum
  | Ln
  deriving (Eq, Ord, Show)

-- | Built in binary functions (excluding those naturally expressed by a PairSeq)
data BinaryFunction
  = Power
  deriving (Eq, Ord, Show)

class ContainsFunction e where
  functionName :: e -> String -- ^ Return the name of a contained function

instance ContainsFunction UnaryFunction where
  functionName f = case f of
    Cos -> "cos"
    Sin -> "sin"
    Tan -> "tan"
    Abs -> "abs"
    Signum -> "sgn"
    Ln -> "ln"

instance ContainsFunction BinaryFunction where
  functionName f = case f of
    Power -> "pow"

instance PrettyPrintable e => ContainsFunction (FunctionApplication e) where
  functionName f = case f of
    ApplyUnary fn _ -> functionName fn
    ApplyBinary fn _ _ -> functionName fn
    ApplyUserDefined sym _ -> prettyPrint sym

-- | Wrapper type for different types of function application
data FunctionApplication e
  = ApplyUnary UnaryFunction (Expression e) -- ^ Application of built-in unary function
  | ApplyBinary BinaryFunction (Expression e) (Expression e) -- ^ Application of built-in binary function
  | ApplyUserDefined e [Expression e] -- ^ Application of user-defined function to arbitrary number of parameters
  deriving (Eq, Ord, Show)

-- | Returns the list of all parameters to a function application
functionParams :: FunctionApplication e -> [Expression e]
functionParams a = case a of
  ApplyUnary _ e0 -> [e0]
  ApplyBinary _ e0 e1 -> [e0, e1]
  ApplyUserDefined _ params -> params

-- | Converts a special value to its floating point equivalent
fromSpecial :: (Floating a) => Special -> a
fromSpecial s = case s of
  Euler -> exp 1
  Pi -> pi

-- | A general expression type
data Expression e
  = Symbol e -- ^ A symbol of user-defined type
  | Sum (PairSeq SumTag e) -- ^ A sum of terms multiplied by rational co-efficients plus a constant
  | Product (PairSeq ProdTag e) -- ^ A product of terms raised to rationals multiplied by a constant
  | ConstantFloat Double -- ^ A constant floating-point value
  | ConstantRational Rational -- ^ A constant rational value
  | Diff (Expression e) e Integer -- ^ The nth derivative on an expression w.r.t the specified symbol
  | Int (Expression e) e -- ^ The integral of the expression w.r.t. the specified symbol
  | Application (FunctionApplication e) -- ^ A function application
  | ConstantSpecial Special -- ^ A special constant
  deriving (Show, Eq, Ord)

-- | The re-write state of an expression
data RewriteState
  = NonNormal -- ^ The expression has not been simplified
  | Normal -- ^ The expression has been simplified but may contain a top-level multiplier
  | NormalAndExtracted -- ^ The expression has been simplified and the top-level multiplier is one
  deriving (Eq, Ord, Show)

-- | A general representation for summation and products
data PairSeq t e = PairSeq {
  _psOverall :: Rational, -- For products a top-level multiplier, for sums a top-level additive value
  _psTerms :: Map (Expression e) Rational, -- A map of expressions to a multiplier for sums, or power for products
  _psRewriteState :: RewriteState -- The simplification state of the PairSeq
} deriving (Eq, Ord, Show)

Lens.makeLenses ''PairSeq

-- | A type-class for operations defined on both sums and products
class PairSeqLike t e where
  empty :: PairSeq t e -- ^ Constructs a null expression (0 for sums, 1 for products)
  extractMultipliers :: Ord e => PairSeq t e -> PairSeq t e -- ^ Simplifies sub-terms, extracts their top-level multipliers and integrates them into the PairSeq
  wrap :: PairSeq t e -> Expression e -- ^ Converts a PairSeq back into an expression
  addTerm :: Ord e => Expression e -> Rational -> PairSeq t e -> PairSeq t e -- Adds a term to the map

-- | Is the PairSeq simplified?
isNormalised :: PairSeq t e -> Bool
isNormalised seq' = case _psRewriteState seq' of
  Normal -> True
  NormalAndExtracted -> True
  _ -> False

-- | Simplifies a PairSeq
normalise :: (PairSeqLike t e, Ord e) => PairSeq t e -> PairSeq t e
normalise seq' = if isNormalised seq'
  then seq'
  else Lens.set psRewriteState Normal . removeZeros $ extractMultipliers seq'

-- | Given an expression, extracts a top-level multiplier and a rewritten expression e.g. 3x^2 -> (3, x^2)
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

-- | Incremnts a term within a PairSeq (utility function for addTerm)
incrementTerm :: Ord e => Expression e -> Rational -> PairSeq t e -> PairSeq t e
incrementTerm expr coeff = Lens.over psTerms $ Map.insertWith (+) expr coeff

-- | Converts a PairSeq to a list of expression and associated rational multipler or power.
toPairs :: PairSeqLike t e => PairSeq t e -> [(Expression e, Rational)]
toPairs seq' = if hasNullOverall seq'
  then terms
  else overall:terms
  where
  overall = (ConstantRational $ _psOverall seq', 1)
  terms = Map.assocs $ _psTerms seq'

-- | Converts a list of expressions and associated values back to a PairSeq
fromPairs :: (Ord e, PairSeqLike t e) => [(Expression e, Rational)] -> PairSeq t e
fromPairs = foldl' (flip $ uncurry addTerm) empty

-- | Removes all terms whose associated co-efficient is zero from a PairSeq
removeZeros :: PairSeq t e -> PairSeq t e
removeZeros = Lens.over psTerms $ Map.filter (/= 0)

-- | Adds two expressions (without simplifying)
add :: Ord e => Expression e -> Expression e -> Expression e
add a b = Sum $ fromPairs [(a, 1), (b, 1)]

-- | Subtract two expressions (without simplifying)
sub :: Ord e => Expression e -> Expression e -> Expression e
sub a b = Sum $ fromPairs [(a, 1), (b, -1)]

-- | Multiply two expressions (without simplifying)
mul :: Ord e => Expression e -> Expression e -> Expression e
mul a b = Product $ fromPairs [(a, 1), (b, 1)]

-- | Divide two expressions (without simplifying)
divide :: Ord e => Expression e -> Expression e -> Expression e
divide a b = Product $ fromPairs [(a, 1), (b, -1)]

-- | Raise expression to a rational value (without simplifying)
raise :: Ord e => Expression e -> Rational -> Expression e
raise a b = Product $ fromPairs [(a, b)]

-- | Returns true if the overall coefficient for a PairSeq is the default value.
-- This is 0 for sums and 1 for products.
hasNullOverall :: forall t e . PairSeqLike t e => PairSeq t e -> Bool
hasNullOverall seq' = _psOverall seq' == _psOverall (empty :: PairSeq t e)

-- | Simplifies a PairSeq
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

-- | Simplifies an expression as much as possible
simplify :: Ord e => Expression e -> Expression e
simplify (Product seq') = simplify' $ simplifyPairSeq seq'
simplify (Sum seq') = simplify' $ simplifyPairSeq seq'
simplify (Application (ApplyUnary Abs e)) = simplify' . Application . ApplyUnary Abs $ simplify e
simplify (Application (ApplyUnary Signum e)) = simplify' . Application . ApplyUnary Signum $ simplify e
simplify (Application (ApplyBinary Power a b)) = simplify' . Application $ ApplyBinary Power (simplify a) (simplify b)
simplify e = simplify' e

-- | Simplifies expressions using a rewrite rules at the top level
simplify' :: Ord e => Expression e -> Expression e
simplify' (Application (ApplyUnary Abs (ConstantRational r))) = ConstantRational $ abs r
simplify' (Application (ApplyUnary Abs (ConstantFloat f))) = ConstantFloat $ abs f
simplify' (Application (ApplyUnary Signum (ConstantRational r))) = ConstantRational $ signum r
simplify' (Application (ApplyUnary Signum (ConstantFloat f))) = ConstantFloat $ signum f
simplify' (Diff e sym i) = genericIndex (iterate (diff' sym) e) i
simplify' (Int e sym) = integrate' sym e
simplify' expr@(Product seq') = case _psOverall seq' of
  0 -> 0
  _ -> expr
simplify' (Application (ApplyBinary Power f (ConstantRational g))) = simplify $ raise f g
simplify' e = e

-- | Applies a rewrite function to an expression and its sub-expressions
rewrite :: Ord e => (Expression e -> Expression e) -> Expression e -> Expression e
rewrite f = simplify' . f . rewrite' f

-- | Applies a rewrite to the sub-expressions of a PairSeq
rewritePairSeq :: (Ord e, PairSeqLike t e) => (Expression e -> Expression e) -> PairSeq t e -> PairSeq t e
rewritePairSeq f = foldl' addTerm' empty . toPairs
  where
  addTerm' s (e, r) = addTerm (rewrite f e) r s

-- | Helper functions for rewrite
rewrite' :: Ord e => (Expression e -> Expression e) -> Expression e -> Expression e
rewrite' f (Sum seq') = simplifyPairSeq $ rewritePairSeq f seq'
rewrite' f (Product seq') = simplifyPairSeq $ rewritePairSeq f seq'
rewrite' f (Diff e sym n) = Diff (simplify' $ rewrite f e) (rewriteSymbol f sym) n
rewrite' f (Int e sym) = Int (simplify' $ rewrite f e) (rewriteSymbol f sym)
rewrite' f (Application a) = Application $ case a of
  ApplyUnary fn e0 -> ApplyUnary fn (simplify' $ rewrite f e0)
  ApplyBinary fn e0 e1 -> ApplyBinary fn (simplify' $ rewrite f e0) (simplify' $ rewrite f e1)
  ApplyUserDefined sym params -> ApplyUserDefined (rewriteSymbol f sym) $ (simplify' . rewrite f) <$> params
rewrite' _ e@(Symbol _) = e
rewrite' _ e@(ConstantFloat _) = e
rewrite' _ e@(ConstantRational _) = e
rewrite' _ s@(ConstantSpecial _) = s

-- | Helper function used to rewrite a symbol using a general expression rewrite function
rewriteSymbol :: (Expression e -> Expression f) -> e -> f
rewriteSymbol f sym = case f (Symbol sym) of
  Symbol s -> s
  _ -> error "rewriteSymbol: cannot subsitute variable with complex expression"

-- | Replaces an expression with a new one within the supplied expression
subst :: Ord e => Expression e -> Expression e -> Expression e -> Expression e
subst from to = simplify . rewrite update
  where
  update e' = if e' == from then to else e'

-- | Uses the supplied function to rewrite symbols within an expression
substSymbols :: (Ord e, Ord f) => (e -> f) -> Expression e -> Expression f
substSymbols f = expandSymbols (Symbol . f)

-- | Uses the supplied function to expand symbools to an expression
expandSymbols :: (Ord e, Ord f) => (e -> Expression f) -> Expression e -> Expression f
expandSymbols f expr = simplify $ case expr of
  Symbol s -> f s
  ConstantFloat r -> ConstantFloat r
  ConstantRational r -> ConstantRational r
  Sum seq' -> Sum . fromPairs $ transformPair <$> toPairs seq'
  Product seq' -> Product . fromPairs $ transformPair <$> toPairs seq'
  Diff e sym i -> Diff (expandSymbols f e) (replaceSymbol sym) i
  Int e sym -> Int (expandSymbols f e) (replaceSymbol sym)
  Application a -> Application $ case a of
    ApplyUnary fn e0 -> ApplyUnary fn (expandSymbols f e0)
    ApplyBinary fn e0 e1 -> ApplyBinary fn (expandSymbols f e0) (expandSymbols f e1)
    ApplyUserDefined sym params -> ApplyUserDefined (replaceSymbol sym) (expandSymbols f <$> params)
  ConstantSpecial s -> ConstantSpecial s
  where
  transformPair (e, r) = (expandSymbols f e, r)
  replaceSymbol sym = case f sym of
    Symbol s -> s
    _ -> error "expandSymbols: cannot substitute variable with complex expression"

-- | Constructs a 1D Lagrange interpolation though the specified (x, val) pairs
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

-- | Returns true if the expression depends on any symbol in the supplied set
depends :: Ord e => Set e -> Expression e -> Bool
depends syms (Symbol s) = Set.member s syms
depends syms (Sum seq') =
  foldl (||) False $ map (\(a, _) -> depends syms a) $ toPairs seq'
depends syms (Product seq') =
  foldl (||) False $ map (\(a, _) -> depends syms a) $ toPairs seq'
depends _ (ConstantFloat _) = False
depends _ (ConstantRational _) = False
depends _ (ConstantSpecial _) = False
depends syms (Diff e _sym _) = depends syms e
depends syms (Int e sym) = Set.member sym syms || depends syms e
depends syms (Application a) = case a of
  ApplyUnary _ e0 -> depends syms e0
  ApplyBinary _ e0 e1 -> depends syms e0 || depends syms e1
  ApplyUserDefined sym params -> foldl (||) (Set.member sym syms) (depends syms <$> params)

-- | Returns all symbols this expression requires for evaluation
vars :: Ord e => Expression e -> Set e
vars (Symbol s) = Set.singleton s
vars (Sum seq') =
  foldl (Set.union) Set.empty $ map (\(a, _) -> vars a) (toPairs seq')
vars (Product seq') =
  foldl (Set.union) Set.empty $ map (\(a, _) -> vars a) (toPairs seq')
vars (ConstantFloat _) = Set.empty
vars (ConstantRational _) = Set.empty
vars (ConstantSpecial _) = Set.empty
vars (Diff e _sym _) = vars e
vars (Int e sym) = Set.insert sym (vars e)
vars (Application a) = case a of
  ApplyUnary _ e0 -> vars e0
  ApplyBinary _ e0 e1 -> Set.union (vars e0) (vars e1)
  ApplyUserDefined sym params -> foldl Set.union (Set.singleton sym) (vars <$> params)

-- | Given an expression, symbol and power, attempts to determine a coefficient expression.
-- e.g. polyCoeff (ax^2 + bx + 5) x 1 == b
polyCoeff :: Ord e => Expression e -> e -> Int -> Maybe (Expression e)
polyCoeff expr sym power = (!! power) <$> (++ repeat 0) <$> polyCoeffs expr sym

-- | Given an expression and a symbol, determines a co-efficient for each power of that symbol
-- e.g PolyCoeffs (ax^2 + bx + 5) x -> [5, b, a]
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
  polyCoeffs' s@(ConstantSpecial _) = Just [s]
  polyCoeffs' (Diff _expr _sym _power) = Nothing
  polyCoeffs' (Int _expr _sym) = Nothing
  polyCoeffs' (Application _) = Nothing

-- | Extracts an element from a list at the specified index and returns the
-- element and the list with that element removed.
extractElem :: [a] -> Int -> (a, [a])
extractElem lst index = (elem', rest)
  where
  (header, elem' : footer) = splitAt index lst
  rest = header ++ footer

-- | Calculates the derivative of an expression w.r.t. the specified symbol
diff :: Ord e => e -> Expression e -> Expression e
diff sym = simplify . diff' sym

-- | Helper function for diff
diff' :: Ord e => e -> Expression e -> Expression e
diff' sym expression = case expression of
  ConstantFloat _ -> 0
  ConstantRational _ -> 0
  ConstantSpecial _ -> 0
  Symbol s -> if s == sym then 1 else 0
  Application a -> case a of
    ApplyUnary Abs x -> (Application $ ApplyUnary Signum x) * diff sym x
    ApplyUnary Signum _ -> 0
    ApplyUnary Ln x -> raise x (-1) * diff sym x
    ApplyUnary Sin x -> (Application $ ApplyUnary Cos x) * diff sym x
    ApplyUnary Cos x -> - (Application $ ApplyUnary Sin x) * diff sym x
    ApplyBinary Power f g -> expression * b
      where
      b = (diff sym f) * g / f + (diff sym g) * (Application $ ApplyUnary Ln f)
    _ -> diffNoun
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
  where
  diffNoun = if depends (Set.singleton sym) expression
    then Diff expression sym 1
    else 0

-- | Given a PairSeq, split into two lists of expression-rational pairs.
-- The first depends on the given symbol, the second does not.
splitPairSeqDepends :: (PairSeqLike t e, Ord e) => Set e -> PairSeq t e -> ([(Expression e, Rational)], [(Expression e, Rational)])
splitPairSeqDepends syms seq' = foldl' combine ([], []) $ toPairs seq'
  where
  combine (dep, nodep) (expr, coeff) = if depends syms expr
    then (addTerm' dep, nodep)
    else (dep, addTerm' nodep)
    where
    addTerm' terms = (expr, coeff):terms

-- | Calculates the definite integral of an expression at the specified limits
definiteIntegrate :: Ord e => e -> Expression e -> Expression e -> Expression e -> Expression e
definiteIntegrate sym low high =
  differenceAtLimits sym low high . integrate sym

-- | Calculates the difference between two expressions at the specified limits
differenceAtLimits :: Ord e => e -> Expression e -> Expression e -> Expression e -> Expression e
differenceAtLimits sym low high expr =
  expand $ (exprAt high) - (exprAt low)
  where
  exprAt pos = subst (Symbol sym) pos expr

-- | Calculates an indefinite integral (but does not add a c term)
integrate :: Ord e => e -> Expression e -> Expression e
integrate sym = simplify . integrate' sym . expand

-- | Helper function for integrate
integrate' :: Ord e => e -> Expression e -> Expression e
integrate' sym expr = case expr of
  Application (ApplyUnary Abs _) -> integrateByParts sym expr 1
  Application (ApplyUnary Signum _) -> integrateByParts sym expr 1
  Application (ApplyUnary Ln _) -> integrateByParts sym expr 1
  ConstantFloat _ -> Symbol sym * expr
  ConstantRational _ -> Symbol sym * expr
  s@(ConstantSpecial _) -> Symbol sym * s
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
  Application _ -> intNoun
  where
  intNoun = if depends (Set.singleton sym) expr
    then Int expr sym
    else Symbol sym * expr

-- | Helper function to integrate a list of terms raised to rationals
integrateProductSeq :: Ord e => e -> [(Expression e, Rational)] -> Expression e
integrateProductSeq sym [] = Symbol sym
integrateProductSeq sym [(expr, exp')]
  | exp' == 0       = symExpr
  | expr == symExpr = if exp' /= -1
                        then raise symExpr (exp' + 1) / fromRational (exp' + 1)
                        else Application $ ApplyUnary Ln symExpr
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

-- | Integrates the product of the two expressions by parts where the first expression
-- is the one that is repeatedly differentiated.
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

-- | Repeatedly rewrites an expression until it stops changing (this is a part of a hack, because expand is hard)
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

-- | Rewrites an expression to an expanded form
expand :: Ord e => Expression e -> Expression e
expand = rewriteFixedPoint expand'

-- | Helper function for expand
expand' :: Ord e => Expression e -> Expression e
expand' (Product seq') = constructExpandedProduct $ toPairs seq'
expand' x = x

-- | Multiplies a PairSeq SumTag by the given expression
mulSum :: Ord e => PairSeq SumTag e -> Expression e -> PairSeq SumTag e
mulSum seq' (Sum seq'') = fromPairs result
  where
  result = do
    (e1, c1) <- toPairs seq'
    (e2, c2) <- toPairs seq''
    return (e1 * e2, c1 * c2)
mulSum seq' expr = fromPairs $ (\(a,b) -> (a * expr, b)) <$> toPairs seq'

-- | Multiplies the list of terms and powers by the given expression
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

-- | A wrapper for symbols used during rewrites
data TempSymbol e
  = Original e
  | Temporary String
  deriving (Eq, Ord, Show)

-- | Wrap a user-defined symbol
toTempSymbol :: e -> TempSymbol e
toTempSymbol s = Original s

-- | Unwrap a user-defined symbol
fromTempSymbol :: TempSymbol e -> e
fromTempSymbol (Original s) = s
fromTempSymbol (Temporary s) = error $ "fromTempSymbol: unexpected symbol " ++ s ++ " in expression"

-- | Derive an explicit time-stepping scheme using the given delta-t, previous value and first-derivatives
adamsBashforth :: Ord e => e -> Expression e -> [Expression e] -> Expression e
adamsBashforth = adamsBashforthGeneral 1

-- | Derive an explicit time-stepping scheme using the previous value and supplied nth-derivatives
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
  abs = simplify . Application . ApplyUnary Abs
  signum = simplify . Application . ApplyUnary Signum

instance Ord e => Fractional (Expression e) where
  fromRational = ConstantRational
  (/) a = simplify . divide a

instance Ord e => Floating (Expression e) where
  pi = ConstantSpecial Pi
  exp = simplify . Application . ApplyBinary Power (ConstantSpecial Euler)
  log = simplify . Application . ApplyUnary Ln
  (**) a = simplify . Application . ApplyBinary Power a
  sin = simplify . Application . ApplyUnary Sin
  cos = simplify . Application . ApplyUnary Cos
  asin = error "asin: unimplemented"
  acos = error "acos: unimplemented"
  atan = error "atan: unimplemented"
  sinh = error "sinh: unimplemented"
  cosh = error "cosh: unimplemented"
  asinh = error "asinh: unimplemented"
  acosh = error "acosh: unimplemented"
  atanh = error "atanh: unimplemented"

powPrec = PrecLevel 4
unaryPrec = PrecLevel 3
mulPrec = PrecLevel 2
addPrec = PrecLevel 1

instance PrettyPrintable e => PrettyPrintable (Expression e) where
  toDoc expr = pDoc $ toPDoc expr
    where
    renderFloat :: Float -> PDoc
    renderFloat i = if i >= 0
      then PDoc (toDoc i) NoAssoc AtomPrec
      else PDoc (hcat [char '-', toDoc (-i)]) NoAssoc unaryPrec
    renderInteger :: Integer -> PDoc
    renderInteger = renderFloat . fromIntegral
    renderRational r = case denominator r of
       1 -> renderInteger $ numerator r
       _ -> renderDivision (renderInteger $  numerator r) (renderInteger $ denominator r)
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
      Diff e s i -> renderFunction "diff" $ [toPDoc e, renderTerminal s] ++ if i == 1
        then []
        else [renderTerminal i]
      Int e s -> renderFunction "int" [toPDoc e, renderTerminal s]
      Application a -> renderFunction (functionName a) (toPDoc <$> (functionParams a))
      ConstantSpecial s -> renderSpecial s
    renderSpecial s = case s of
      Euler -> renderTerminal "e"
      Pi -> renderTerminal "pi"
