module FDGEN.Stencil where
import Control.Applicative ((<$>))
import Control.Exception (assert)
import Data.Char (chr, ord)
import Data.List (genericIndex, genericLength)
import FDGEN.Algebra (Expression(..), expand, lagrange, subst, substSymbols, diff)
import FDGEN.Pretty (PrettyPrintable, toDoc)
import Text.PrettyPrint as PrettyPrint

data Stagger = StaggerPos | StaggerNeg | StaggerNone
  deriving (Eq, Ord, Show)

data StencilSpec = StencilSpec
  { _stencilSpecOrder :: Integer
  , _stencilSpecDerivatives :: [Integer]
  , _stencilSpecStaggering :: [Stagger]
  } deriving Show


data Stencil = Stencil
  { _stencilDimensions :: [Integer]
  , _stencilValues :: [Double]
  , _stencilOrigin :: [Integer]
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
  result = error . show . toDoc $ expand $ interpolatedValue
  derivatives = _stencilSpecDerivatives spec
  staggering = _stencilSpecStaggering spec
  numDimensions = genericLength derivatives
  order = _stencilSpecOrder spec
  stencilWidth stagger derivative = if stagger == StaggerNone && derivative == 0
    then 1
    else order + derivative
  stencilWidths = uncurry stencilWidth <$> zip staggering derivatives
  originGrid = (`div` 2) <$> stencilWidths
  originSubstitutions = (\(dim, gridpoint) -> (Symbol $ Position dim, fromInteger gridpoint * (Symbol $ GridSpacing dim))) <$> zip [0..numDimensions - 1] originGrid
  interpolation = buildInterpolation stencilWidths
  diff' sym power expression = genericIndex (iterate (diff sym) expression) power
  differentiatedInterpolation = foldl (\expr (dim, power) -> diff' (Position dim) power expr) interpolation (zip [0..numDimensions-1] derivatives)
  interpolatedValue = foldl (\expr (from, to) -> subst from to expr) differentiatedInterpolation originSubstitutions
  --result = Stencil
  --  { _stencilDimensions = stencilWidths
  --  , _stencilValues = error "unimplemented"
  --  , _stencilOrigin = originGrid
  --  }
  buildInterpolation :: [Integer] -> Expression StencilTerminal
  buildInterpolation = buildInterpolation' . reverse
    where
    buildInterpolation' [] = Symbol $ FieldValue []
    buildInterpolation' (width:widths) = lagrange (Symbol $ Position dimension) extruded
      where
      dimension = 0 + genericLength widths
      lowerInterpolation = buildInterpolation' widths
      extrude :: Integer -> StencilTerminal -> StencilTerminal
      extrude position (FieldValue positions) = FieldValue (position:positions)
      extrude _ terminal = terminal
      extrudeExpression position = substSymbols (extrude position)
      extruded = [(fromInteger i * (Symbol $ GridSpacing dimension), extrudeExpression i lowerInterpolation) | i <- [0..width - 1]]
