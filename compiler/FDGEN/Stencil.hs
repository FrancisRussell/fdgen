module FDGEN.Stencil where
import Control.Applicative ((<$>))
import Control.Exception (assert)
import Data.Char (chr, ord)
import Data.List (genericIndex, genericLength)
import Data.Map (Map)
import FDGEN.Algebra (Expression(..), expand, lagrange, subst, substSymbols, diff, vars, polyCoeff)
import FDGEN.Pretty (PrettyPrintable, toDoc)
import Text.PrettyPrint as PrettyPrint
import qualified Data.Set as Set
import qualified Data.Map as Map

data Stagger = StaggerPos | StaggerNeg | StaggerNone
  deriving (Eq, Ord, Show)

data StencilSpec = StencilSpec
  { _stencilSpecOrder :: Integer
  , _stencilSpecDerivatives :: [Integer]
  , _stencilSpecStaggering :: [Stagger]
  } deriving Show


data Stencil = Stencil
  { _stencilDimension :: Integer
  , _stencilValues :: Map [Integer] Rational
  } deriving Show

data StencilTerminal
  = GridSpacing Integer
  | Position Integer
  | FieldValue [Integer]
  deriving (Eq, Ord, Show)

instance PrettyPrintable StencilTerminal
  where
  toDoc t = case t of
    GridSpacing i -> PrettyPrint.hcat [toDoc "d", toDoc $ indexToString i]
    Position i -> toDoc $ indexToString i
    FieldValue indices -> hcat
      [ toDoc "stencil["
      , hcat . PrettyPrint.punctuate (toDoc ",") $ toDoc <$> indices
      , toDoc "]"
      ]
    where
    indexToString i = [chr $ ord 'x' + fromIntegral i]

buildStencil :: StencilSpec -> Stencil
buildStencil spec = assert (length derivatives == length staggering) result
  where
  result = expressionToStencil numDimensions interpolatedValue
  derivatives = _stencilSpecDerivatives spec
  staggering = _stencilSpecStaggering spec
  numDimensions = genericLength derivatives
  order = _stencilSpecOrder spec
  stencilWidth stagger derivative = if stagger == StaggerNone && derivative == 0
    then 1
    else order + derivative
  stencilWidths = uncurry stencilWidth <$> zip staggering derivatives
  originGrid = (`div` 2) <$> stencilWidths
  originSubstitutions = (\dim -> (Symbol $ Position dim, 0)) <$> [0..numDimensions - 1]
  interpolation = buildInterpolation $ zip3 stencilWidths originGrid staggering
  diff' sym power expression = genericIndex (iterate (diff sym) expression) power
  differentiatedInterpolation = foldl (\expr (dim, power) -> diff' (Position dim) power expr) interpolation (zip [0..numDimensions-1] derivatives)
  interpolatedValue = foldl (\expr (from, to) -> subst from to expr) differentiatedInterpolation originSubstitutions
  buildInterpolation :: [(Integer, Integer, Stagger)] -> Expression StencilTerminal
  buildInterpolation = buildInterpolation' . reverse
    where
    buildInterpolation' [] = Symbol $ FieldValue []
    buildInterpolation' ((width, centrePoint, stagger):widths) = lagrange (Symbol $ Position dimension) extruded
      where
      dimension = 0 + genericLength widths
      lowerInterpolation = buildInterpolation' widths
      extrude :: Integer -> StencilTerminal -> StencilTerminal
      extrude position (FieldValue positions) = FieldValue (position:positions)
      extrude _ terminal = terminal
      extrudeExpression position = substSymbols (extrude position)
      extruded = [((fromInteger i + staggerOffset stagger), extrudeExpression i lowerInterpolation) | i <- (subtract centrePoint) <$> [0..width - 1]]
      staggerOffset staggering = case staggering of
        StaggerNone -> 0.0
        StaggerPos -> 0.5
        StaggerNeg -> -0.5

expressionToStencil :: Integer -> Expression StencilTerminal -> Stencil
expressionToStencil dim expression = Stencil
  { _stencilDimension = dim
  , _stencilValues = coeffMap
  }
  where
  variables = Set.toList $ vars expression
  coeffs = getCoeff <$> variables
  getCoeff var = case polyCoeff expression var 1 of
    Just (ConstantRational r) -> r
    _ -> error $ "Could not convert expression to stencil coefficient" ++ show expression
  indices = getIndex <$> variables
  getIndex terminal = case terminal of
    FieldValue idx -> idx
    _ -> error $ "Unexpected terminal type in stencil expression " ++ show terminal
  coeffMap = Map.fromList $ zip indices coeffs
