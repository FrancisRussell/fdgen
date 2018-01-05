module FDGEN.Discrete (buildDiscreteForm, buildTemplateDictionary) where
import FDGEN.Algebra (Expression(..), diff, adamsBashforthGeneral, lagrange, expandSymbols, substSymbols, rewriteFixedPoint)
import FDGEN.Tensor (Tensor, TensorIndex)
import FDGEN.Pretty (PrettyPrintable(..), structureDoc, hListDoc, vListDoc)
import FDGEN.Stencil (StencilSpec(..), Stencil(..), buildStencil)
import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)
import Data.List (genericIndex, genericTake, genericDrop)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified FDGEN.Parser as Parser
import qualified FDGEN.Tensor as Tensor
import qualified FDGEN.Template as Template
import qualified Text.PrettyPrint as PrettyPrint

data Terminal
  = FieldRef String TensorIndex
  | ConstantRef String TensorIndex
  | Direction Integer
  deriving (Eq, Ord, Show)

instance PrettyPrintable Terminal
  where
  toDoc t = case t of
    FieldRef name index -> fieldRef name index
    Direction i -> toDoc $ genericIndex ["x", "y", "z"] i
    ConstantRef name index -> fieldRef name index
    where
    fieldRef name index = PrettyPrint.hcat [toDoc name, indexDoc index]
      where
      indexDoc [] = PrettyPrint.empty
      indexDoc indices = toDoc $ show indices

data SemiDiscreteTerminal
  = SemiDiscreteFieldRef String TensorIndex [Integer]
  | SemiDiscreteConstantRef String TensorIndex
  | SemiDiscreteDirection Integer
  deriving (Eq, Ord, Show)

instance PrettyPrintable SemiDiscreteTerminal
  where
  toDoc t = case t of
    SemiDiscreteFieldRef name tensorIndex derivatives -> fieldRef name tensorIndex derivatives
    SemiDiscreteConstantRef name index -> constantRef name index
    SemiDiscreteDirection i -> toDoc $ genericIndex ["x", "y", "z"] i
    where
    fieldRef name tensorIndex derivatives = PrettyPrint.hcat [derivDoc derivatives, toDoc name, indexDoc tensorIndex]
    derivDoc derivatives = if total == 0
      then toDoc ""
      else PrettyPrint.hcat [toDoc "(", toDoc topTerm, toDoc "/", toDoc bottomTerm, toDoc ")" ]
      where
      topTerm = PrettyPrint.hcat $ [ toDoc "d" ] ++ if total == 1 then [] else [toDoc "^", toDoc total]
      bottomTerm = PrettyPrint.hcat $ raiseDiffVar <$> (zip ["x", "y", "z"] derivatives)
      raiseDiffVar (v, p) = case p of
        0 -> toDoc ""
        1 -> PrettyPrint.hcat [toDoc "d", toDoc v]
        _ -> PrettyPrint.hcat [toDoc "d", toDoc v, toDoc "^", toDoc p]
      total = sum derivatives
    constantRef name tensorIndex = PrettyPrint.hcat [toDoc name, indexDoc tensorIndex]
    indexDoc [] = PrettyPrint.empty
    indexDoc indices = toDoc $ show indices

data DiscreteTerminal
  = FieldDataRef String TensorIndex [Integer]
  | ConstantDataRef String TensorIndex
  | GridSpacing Integer
  deriving (Eq, Ord, Show)

instance PrettyPrintable DiscreteTerminal
  where
  toDoc t = case t of
    FieldDataRef name tensorIndex stencilIndex -> fieldRef name tensorIndex stencilIndex
    ConstantDataRef name index -> constantRef name index
    GridSpacing i -> toDoc $ genericIndex ["hx", "hy", "hz"] i
    where
    fieldRef name tensorIndex stencilIndex = PrettyPrint.hcat [toDoc name, indexDoc tensorIndex, indexDoc stencilIndex]
    constantRef name tensorIndex = PrettyPrint.hcat [toDoc name, indexDoc tensorIndex]
    indexDoc [] = PrettyPrint.empty
    indexDoc indices = toDoc $ show indices

data TemporalTerminal
  = PreviousValue
  | DeltaT
  | PreviousDerivative Integer
  deriving (Eq, Ord, Show)

instance PrettyPrintable TemporalTerminal
  where
  toDoc t = case t of
    PreviousValue -> toDoc "y0"
    DeltaT -> toDoc "h"
    PreviousDerivative i -> PrettyPrint.hcat $ toDoc <$> ["f(n-", i', ")"]
      where
      i' = show i

data SpatialTerminal
  = SpatialDelta Integer
  | FieldValue [Integer]
  | Position Integer
  deriving (Eq, Ord, Show)

instance PrettyPrintable SpatialTerminal
  where
  toDoc t = case t of
    SpatialDelta i -> toDoc $ "dx" ++ show i
    FieldValue idx -> toDoc $ "y" ++ show idx
    Position i -> toDoc $ "x" ++ show i

--computeInterpolation :: Integer -> [Bool] -> [Integer] -> Expression SpatialTerminal
--computeInterpolation order staggering derivatives =
--  if length staggering /= length derivatives
--  then error "computeInterpolation: inconsistent dimension specification"
--  else doDerivatives 0 derivatives interpolated
--    where
--    interpolated = buildLagrange 0 stencilWidths []
--    stencilWidths = stencilWidth <$> zip staggering derivatives
--    stencilWidth (stagger, derivative) = 1 + order + derivative
--    buildLagrange _ [] _ = error "buildLangrange: must be called with at least 1 dimension"
--    buildLagrange dim (width:ws) idx =
--      lagrange variable [(pointPos n, pointValue n) | n <- [0 .. width-1]]
--      where
--      variable = Symbol $ Position dim
--      pointPos n = (Symbol $ SpatialDelta dim) * fromInteger n
--      pointValue n = case ws of
--        [] -> Symbol $ FieldValue idx'
--        _ -> buildLagrange (dim+1) ws idx'
--        where
--        idx' = idx ++ [n]
--    doDerivatives _ [] expr = expr
--    doDerivatives dim (d:ds) expr = doDerivatives (dim + 1) ds expr'
--      where
--      expr' = genericIndex (iterate (diff (Position dim)) expr) d

data FieldAccess = FieldAccess {
  _fieldAccessName :: String,
  _fieldAccessTemporalIndex :: Integer,
  _fieldAccessSpatialOffsets :: [Integer]
} deriving Show

data Discretised = Discretised {
  _discretisedMeshes :: [Mesh]
} deriving Show

instance PrettyPrintable Discretised
  where
  toDoc discrete = structureDoc "Discretised"
    [ ("meshes", vListDoc $ _discretisedMeshes discrete)
    ]

data Mesh = Mesh
  { _meshName :: String
  , _meshDimension :: Integer
  , _meshFields :: [Field]
  , _meshSolves :: [Solve]
  } deriving Show

instance PrettyPrintable Mesh
  where
  toDoc mesh = structureDoc "Mesh"
    [ ("name", toDoc $ _meshName mesh)
    , ("dim", toDoc $ _meshDimension mesh)
    , ("fields", vListDoc $ _meshFields mesh)
    , ("solves", vListDoc $ _meshSolves mesh)
    ]

data Field = Field
  { _fieldName :: String
  , _fieldRank :: Integer
  , _fieldSymmetric :: Bool
  , _fieldStaggerSpatial :: [Bool]
  , _fieldStaggerTemporal :: Bool
} deriving Show

instance PrettyPrintable Field
  where
  toDoc field = structureDoc "Field"
    [ ("name", toDoc $ _fieldName field)
    , ("rank", toDoc $ _fieldRank field)
    , ("symmetric", toDoc $ _fieldSymmetric field)
    , ("spatial_staggering", hListDoc $ _fieldStaggerSpatial field)
    , ("temporal_staggering", toDoc $ _fieldStaggerTemporal field)
    ]

data Solve = Solve
  { _solveName :: String
  , _solveSpatialOrder :: Integer
  , _solveTemporalOrder :: Integer
  , _solveUpdates :: [Update]
} deriving Show

instance PrettyPrintable Solve
  where
  toDoc solve = structureDoc "Solve"
    [ ("name", toDoc $ _solveName solve)
    , ("spatial_order", toDoc $ _solveSpatialOrder solve)
    , ("temporal_order", toDoc $ _solveTemporalOrder solve)
    , ("updates", vListDoc $ _solveUpdates solve)
    ]

data Interpolation = Interpolation
  { _interpolationOrder :: Integer
  , _interpolationDerivatives :: [Integer]
  , _interpolationExpression :: Expression SpatialTerminal
  } deriving Show

instance PrettyPrintable Interpolation
  where
  toDoc interp = structureDoc "Interpolation"
    [ ("order", toDoc $ _interpolationOrder interp)
    , ("derivatives", toDoc . show $ _interpolationDerivatives interp)
    , ("expression", toDoc $ _interpolationExpression interp)
    ]

data FieldTemporalDerivative
  = FieldTemporalDerivative String Integer
  deriving (Eq, Ord, Show)

instance PrettyPrintable FieldTemporalDerivative
  where
  toDoc (FieldTemporalDerivative f i) = if i /= 0
    then PrettyPrint.hcat $ toDoc <$> ["(d^", i', " * ", f, ")/dt^", i']
    else toDoc f
    where
    i' = show i

data Update = Update
  { _updateLHS :: FieldTemporalDerivative
  , _updateRHS :: Tensor (Expression Terminal)
  , _updateRHSDiscrete :: Tensor (Expression DiscreteTerminal)
  , _updateTimeSteppingSchemes :: Map Integer (Expression TemporalTerminal)
  } deriving Show

instance PrettyPrintable Update
  where
  toDoc update = structureDoc "Update"
    [ ("lhs", toDoc $ _updateLHS update)
    , ("rhs", toDoc $ _updateRHS update)
    , ("rhs_discrete", toDoc $ _updateRHSDiscrete update)
    , ("time_stepping_schemes", schemesDoc)
    ]
    where
    schemesDoc = structureDoc "Map" schemeEntries
    schemeEntries = transformEntry <$> Map.assocs (_updateTimeSteppingSchemes update)
    transformEntry (order, expr) = (show order, toDoc expr)

buildDiscreteForm :: Parser.FDFL -> Discretised
buildDiscreteForm fdfl = Discretised { _discretisedMeshes = catMaybes maybeMeshes }
  where
  maybeMeshes = maybeBuildMesh <$> Map.elems (Parser.getSymbols fdfl)
  maybeBuildMesh (Parser.MeshDef meshDef) = Just $ buildMesh fdfl meshDef
  maybeBuildMesh _ = Nothing

buildTemplateDictionary :: Discretised -> Template.Dict
buildTemplateDictionary discretised = Template.insert "meshes" (Template.ListVal $ Template.DictVal <$> meshes) Template.emptyDict
  where
  meshes = buildMeshDictionary discretised <$> _discretisedMeshes discretised

buildMeshDictionary :: Discretised -> Mesh -> Template.Dict
buildMeshDictionary discretised mesh = Map.fromList
  [ ("name", Template.StringVal $ _meshName mesh)
  , ("dimension", Template.StringVal . show $ _meshDimension mesh)
  , ("fields", Template.ListVal $ Template.DictVal . buildFieldDictionary discretised mesh <$> _meshFields mesh)
  ]

buildFieldDictionary :: Discretised -> Mesh -> Field -> Template.Dict
buildFieldDictionary _ mesh field = Map.fromList
  [ ("name", Template.StringVal $ _fieldName field)
  , ("num_components", Template.StringVal . show $ num_components)
  ]
  where
  num_components = (_meshDimension mesh) ^ (_fieldRank field)

buildMesh :: Parser.FDFL -> Parser.Mesh -> Mesh
buildMesh fdfl mesh = Mesh
  { _meshName = Parser.stringLiteralValue $ Parser._meshName mesh
  , _meshDimension = dimension
  , _meshFields = (buildField fdfl mesh . Parser.getFieldDef fdfl) <$> (Parser._meshFields mesh)
  , _meshSolves = (buildSolve fdfl mesh . getSolveDef) <$> (Parser._meshSolves mesh)
  }
  where
  dimension = Parser._meshDimension mesh
  getSolveDef ident = case Parser.getSymbol fdfl ident of
    Just (Parser.SolveDef solve) -> solve
    _ -> error $ "buildMesh: unknown solve " ++ Parser.identifierValue ident

buildField :: Parser.FDFL -> Parser.Mesh -> Parser.Field -> Field
buildField _ _ field = Field
  { _fieldName = Parser.stringLiteralValue $ Parser._fieldName field
  , _fieldRank = Parser._fieldRank field
  , _fieldSymmetric = Parser._fieldSymmetric field
  , _fieldStaggerSpatial = [] -- TODO: implement me
  , _fieldStaggerTemporal = False -- TODO: implement me
  }

buildSolve :: Parser.FDFL -> Parser.Mesh -> Parser.Solve -> Solve
buildSolve fdfl mesh solve = Solve
  { _solveName = Parser.stringLiteralValue $ Parser._solveName solve
  , _solveSpatialOrder = Parser._solveSpatialOrder solve
  , _solveTemporalOrder = temporalOrder
  , _solveUpdates = (buildUpdate fdfl mesh solve . getExpressionDef) <$> (Parser._solveEquations solve)
  }
  where
  temporalOrder = Parser._solveTemporalOrder solve
  getExpressionDef ident = case Parser.getSymbol fdfl ident of
    Just (Parser.EquationDef equ) -> equ
    _ -> error $ "buildSolve: unknown equation " ++ Parser.identifierValue ident

buildAdamsBashForth :: Integer -> Integer -> Expression TemporalTerminal
buildAdamsBashForth numDerivatives order =
  adamsBashforthGeneral numDerivatives DeltaT (Symbol PreviousValue) $ (Symbol . PreviousDerivative) <$> [0 .. order - 1]

-- Transform derivatives to new terminals that know about them and eliminate function representations.
makeSemiDiscrete :: Integer -> Expression Terminal -> Expression SemiDiscreteTerminal
makeSemiDiscrete dimension expr = rewritten''
  where
  rewritten = substSymbols rewriteTerminal expr
  rewritten' = rewriteFixedPoint collapseDerivative rewritten
  rewritten'' = rewriteFixedPoint removeFunctions rewritten'
  rewriteTerminal t = case t of
    ConstantRef name tensorIndex -> SemiDiscreteConstantRef name tensorIndex
    FieldRef name tensorIndex -> SemiDiscreteFieldRef name tensorIndex (genericTake dimension $ repeat 0)
    Direction i -> SemiDiscreteDirection i
  collapseDerivative e = case e of
    Diff (Function (SemiDiscreteFieldRef name tensorIndex derivatives) dims) (SemiDiscreteDirection d) n ->
      Function (SemiDiscreteFieldRef name tensorIndex derivatives') dims
      where
      derivatives' = (genericTake d derivatives) ++ [n + genericIndex derivatives d] ++ (genericDrop d derivatives)
    _ -> e
  removeFunctions e = case e of
    Function sym@(SemiDiscreteFieldRef _ _ _) _ -> Symbol sym
    _ -> e

buildRHSDiscrete :: Integer -> Expression Terminal -> Expression DiscreteTerminal
buildRHSDiscrete dimension expr = error $ show $ prettyPrint $ expandSymbols makeDiscrete semiDiscrete
  where
  semiDiscrete = makeSemiDiscrete dimension expr
  makeDiscrete :: SemiDiscreteTerminal -> Expression DiscreteTerminal
  makeDiscrete sym = case sym of
    SemiDiscreteConstantRef name tensorIndex -> Symbol $ ConstantDataRef name tensorIndex
    SemiDiscreteFieldRef name tensorIndex derivatives -> stencilExpression
      where
      stencilExpression = foldl (+) 0 terms
      terms = buildTerm <$> Map.toList (_stencilValues stencil)
      buildTerm (index, coeff) = (fromRational coeff) * (Symbol $ FieldDataRef name tensorIndex index)
      stencilSpec = StencilSpec
        { _stencilSpecOrder = error "unknown order"
        , _stencilSpecDerivatives = derivatives
        , _stencilSpecStaggering = error "unknown staggering"
        }
      stencil = buildStencil stencilSpec
    SemiDiscreteDirection _ -> error "Unexpected SemiDiscreteDirection found (should have been eliminated)."

buildUpdate :: Parser.FDFL -> Parser.Mesh -> Parser.Solve -> Parser.Equation -> Update
buildUpdate fdfl mesh solve equ = Update
  { _updateLHS = lhs
  , _updateRHS = rhs
  , _updateRHSDiscrete = rhsDiscrete
  , _updateTimeSteppingSchemes = timestepping
  }
  where
  getFieldName = Parser.stringLiteralValue . Parser._fieldName
  dimension = Parser._meshDimension mesh
  --order = Parser._solveSpatialOrder solve
  lhs = buildLHS $ Parser._fieldUpdateLHS equ
  rhs = buildRHS $ Parser._fieldUpdateRHS equ
  rhsDiscrete = buildRHSDiscrete dimension <$> (buildRHS $ Parser._fieldUpdateRHS equ)
  temporalOrder = Parser._solveTemporalOrder solve
  timestepping = case lhs of
    FieldTemporalDerivative _ d -> Map.fromList [(n, buildAdamsBashForth d n) | n <- [1..temporalOrder]]
--  interpolation derivatives = Interpolation
--    { _interpolationOrder = order
--    , _interpolationDerivatives = derivatives
--    , _interpolationExpression = (computeInterpolation order (genericTake dimension $ repeat False) derivatives)
--    }
  buildLHS expr = buildDerivative 0 expr
    where
    buildDerivative n (Parser.FieldTemporalDerivative subExpr) = buildDerivative (n + 1) subExpr
    buildDerivative n (Parser.FieldRef ident) = FieldTemporalDerivative (getFieldName $ Parser.getFieldDef fdfl ident) n
    buildDerivative _ _ = error $ "Unsupported LHS: " ++ show expr
  buildRHS :: Parser.FieldExpr Parser.Identifier -> Tensor (Expression Terminal)
  buildRHS expr = case expr of
    Parser.FieldTemporalDerivative _ -> error "buildUpdate: Temporal derivative not expected in RHS"
    Parser.FieldAddition l r -> Tensor.add (buildRHS l) (buildRHS r)
    Parser.FieldInner l r -> Tensor.inner (buildRHS l) (buildRHS r)
    Parser.FieldOuter l r -> Tensor.outer (buildRHS l) (buildRHS r)
    Parser.FieldDot l r -> Tensor.dot (buildRHS l) (buildRHS r)
    Parser.FieldDivision l r -> Tensor.divide (buildRHS l) (buildRHS r)
    Parser.FieldGradient t -> Tensor.outerWithOp ($) nabla (buildRHS t)
    Parser.FieldDivergence t -> Tensor.dotWithOp ($) (+) nabla (buildRHS t)
    Parser.FieldSpatialDerivative t i -> genDerivative i <$> (buildRHS t)
    Parser.FieldLiteral literal -> case literal of
      Parser.ScalarConstant s -> Tensor.constructTensor dimension 0 [ConstantFloat s]
      Parser.PermutationSymbol -> genLC dimension
    Parser.FieldRef ref -> case Parser.getSymbol fdfl ref of
      Just (Parser.ConstantDef constant) ->
        buildAccessTensor (Parser._constantName constant) (Parser._constantRank constant) constructor
          where
          constructor name = Symbol . ConstantRef name
      Just (Parser.FieldDef field) ->
        buildAccessTensor (Parser._fieldName field) (Parser._fieldRank field) constructor
          where
          constructor name index = Function (FieldRef name index) directions
      Just (Parser.FieldExprDef def) -> buildRHS def
      Just _ -> error $ "buildUpdate: unable to treat symbol as field: " ++ Parser.identifierValue ref
      Nothing -> error $ "buildUpdate: unknown symbol " ++ Parser.identifierValue ref
    where
    nabla :: Tensor (Expression Terminal -> Expression Terminal)
    nabla = Tensor.generateTensor dimension 1 genNabla
    genNabla :: TensorIndex -> (Expression Terminal -> Expression Terminal)
    genNabla [dim] = genDerivative dim
    genNabla _ = error "genNabla: called for wrong rank"
    genDerivative :: Integer -> Expression Terminal -> Expression Terminal
    genDerivative dir = diff (Direction dir)
    genLC :: Integer -> Tensor (Expression Terminal)
    genLC dim = case dim of
      2 -> Tensor.constructTensor 2 2 [0, 1, -1, 0]
      3 -> Tensor.generateTensor 3 3 e3D
        where
        e3D idx = case idx of
          [0,1,2] -> 1
          [1,2,0] -> 1
          [2,0,1] -> 1
          [2,1,0] -> -1
          [0,2,1] -> -1
          [1,0,2] -> -1
          _ -> 0
      _ -> error $ "genLC: cannot generate Levi-Civita for dimension " ++ show dim
    buildAccessTensor name rank constructor = Tensor.generateTensor dimension rank gen
      where
      gen idx = constructor (Parser.stringLiteralValue name) idx
    directions = Symbol . Direction <$> [0 .. dimension -1]
