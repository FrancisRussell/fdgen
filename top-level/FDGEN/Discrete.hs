module FDGEN.Discrete (buildDiscreteForm, buildTemplateDictionary
                      , Discretised(..), Mesh(..), Field(..), Solve(..), findFieldUpdate, FieldLValue(..)
                      , numPreviousTimestepsNeeded, DiscreteTerminal(..), Update(..)
                      , scalarizeTensorFields, constantFoldDiscretised
                      , maxTimestepOrder, getTimestepping, TemporalTerminal(..)
                      , BoundaryCondition(..), EdgeDomain(..), solveGetGhostSizes
                      , meshGetField, BoundaryConditionType(..), Terminal(..)
                      , meshGetInitialValue) where
import FDGEN.Algebra (Expression(..), diff, adamsBashforthGeneral, expandSymbols, substSymbols, rewriteFixedPoint
                     , vars, FunctionApplication(..))
import FDGEN.Tensor (Tensor, TensorIndex)
import FDGEN.Pretty (PrettyPrintable(..), structureDoc, hListDoc, vListDoc)
import FDGEN.Stencil (StencilSpec(..), Stencil(..), buildStencil, Stagger(..))
import FDGEN.Util (mergeBoundingRange)
import Control.Applicative ((<$>))
import Control.Exception (assert)
import Data.Maybe (catMaybes)
import Data.List (genericIndex, genericTake, genericDrop, genericReplicate, genericLength, intersperse)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
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
      bottomTerm = PrettyPrint.hcat $ zipWith raiseDiffVar ["x", "y", "z"] derivatives
      raiseDiffVar v p = case p of
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
  | DiscreteDirection Integer
  deriving (Eq, Ord, Show)

instance PrettyPrintable DiscreteTerminal
  where
  toDoc t = case t of
    FieldDataRef name tensorIndex stencilIndex -> fieldRef name tensorIndex stencilIndex
    ConstantDataRef name index -> constantRef name index
    DiscreteDirection index -> toDoc $ genericIndex ["x", "y", "z"] index
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
    PreviousDerivative i -> PrettyPrint.hcat $ toDoc <$> ["f(n-", show i, ")"]

data Discretised = Discretised
  { _discretisedLiterals :: Map String Rational
  , _discretisedMeshes :: [Mesh]
  } deriving Show

instance PrettyPrintable Discretised
  where
  toDoc discrete = structureDoc "Discretised"
    [ ("literals", structureDoc "Map" literalEntries)
    , ("meshes", vListDoc $ _discretisedMeshes discrete)
    ]
    where
    literalEntries = transformEntry <$> Map.assocs (_discretisedLiterals discrete)
    transformEntry (name, value) = (show name, toDoc value)

data Mesh = Mesh
  { _meshName :: String
  , _meshDimension :: Integer
  , _meshFields :: [Field]
  , _meshSolves :: [Solve]
  , _meshGridSpacing :: [Expression DiscreteTerminal]
  , _meshDimensions :: [Expression DiscreteTerminal]
  , _meshInitialValues :: [(String, Tensor (Expression Terminal))]
  } deriving Show

instance PrettyPrintable Mesh
  where
  toDoc mesh = structureDoc "Mesh"
    [ ("name", toDoc $ _meshName mesh)
    , ("dim", toDoc $ _meshDimension mesh)
    , ("fields", vListDoc $ _meshFields mesh)
    , ("solves", vListDoc $ _meshSolves mesh)
    , ("grid_spacing", hListDoc $ _meshGridSpacing mesh)
    , ("dimensions", hListDoc $ _meshDimensions mesh)
    , ("initial", structureDoc "Map" $ (\(a,b) -> (a, toDoc b)) <$> _meshInitialValues mesh)
    ]

data Field = Field
  { _fieldName :: String
  , _fieldRank :: Integer
  , _fieldSymmetric :: Bool
  , _fieldStaggerSpatial :: [[Bool]]
  , _fieldStaggerTemporal :: Bool
} deriving Show

instance PrettyPrintable Field
  where
  toDoc field = structureDoc "Field"
    [ ("name", toDoc $ _fieldName field)
    , ("rank", toDoc $ _fieldRank field)
    , ("symmetric", toDoc $ _fieldSymmetric field)
    , ("spatial_staggering", hListDoc $ hListDoc <$> _fieldStaggerSpatial field)
    , ("temporal_staggering", toDoc $ _fieldStaggerTemporal field)
    ]

data Solve = Solve
  { _solveName :: String
  , _solveSpatialOrder :: Integer
  , _solveTemporalOrder :: Integer
  , _solveUpdates :: [Update]
  , _solveBoundaryConditions :: [BoundaryCondition]
  , _solveDeltaT :: Expression DiscreteTerminal
} deriving Show

instance PrettyPrintable Solve
  where
  toDoc solve = structureDoc "Solve"
    [ ("name", toDoc $ _solveName solve)
    , ("spatial_order", toDoc $ _solveSpatialOrder solve)
    , ("temporal_order", toDoc $ _solveTemporalOrder solve)
    , ("updates", vListDoc $ _solveUpdates solve)
    , ("boundary_conditions", vListDoc $ _solveBoundaryConditions solve)
    , ("delta_t", toDoc $ _solveDeltaT solve)
    ]

data BoundaryConditionType
  = Dirichlet
  | Neumann
  deriving (Eq, Ord, Show)

data EdgeDomain
  = AllExteriorEdges
  | TaggedEdgeString String
  deriving (Eq, Ord, Show)

instance PrettyPrintable EdgeDomain
  where
  toDoc = toDoc . show

instance PrettyPrintable BoundaryConditionType
  where
  toDoc ty = toDoc $ case ty of
    Dirichlet -> "Dirichlet"
    Neumann -> "Neumann"

data FieldLValue
  = FieldLValue String [Integer]
  deriving (Eq, Ord, Show)

instance PrettyPrintable FieldLValue
  where
  toDoc (FieldLValue name indices) = case indices of
   [] -> toDoc name
   _ -> PrettyPrint.hcat [toDoc name, hListDoc indices]

data FieldTemporalDerivative
  = FieldTemporalDerivative FieldLValue Integer
  deriving (Eq, Ord, Show)

instance PrettyPrintable FieldTemporalDerivative
  where
  toDoc (FieldTemporalDerivative f i) = if i /= 0
    then PrettyPrint.hcat [toDoc "(d^", toDoc i, toDoc " * ", toDoc f, toDoc ")/dt^", toDoc i]
    else toDoc f

class ContainsLValue e where
  getLValue :: e -> FieldLValue

instance ContainsLValue FieldTemporalDerivative
  where
  getLValue (FieldTemporalDerivative lvalue _) = lvalue

instance ContainsLValue Update
  where
  getLValue = getLValue . _updateLHS

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

data BoundaryCondition = BoundaryCondition
  { _bcType :: BoundaryConditionType
  , _bcField :: FieldLValue
  , _bcRHS :: Tensor (Expression Terminal)
  , _bcRHSDiscrete :: Tensor (Expression DiscreteTerminal)
  , _bcSubdomains :: [EdgeDomain]
  } deriving Show

instance PrettyPrintable BoundaryCondition
  where
  toDoc bc = structureDoc "BoundaryCondition"
    [ ("type", toDoc $ _bcType bc)
    , ("field", toDoc $ _bcField bc)
    , ("rhs", toDoc $ _bcRHS bc)
    , ("rhs_discrete", toDoc $ _bcRHSDiscrete bc)
    , ("subdomains", vListDoc $ _bcSubdomains bc)
    ]

isMeshConstant :: Expression DiscreteTerminal -> Bool
isMeshConstant expr = foldl (&&) True (isConstantSymbol <$> unknowns)
  where
  unknowns = Set.toList $ vars expr
  isConstantSymbol s = case s of
    FieldDataRef _ _ _ -> False
    ConstantDataRef _ _ -> True
    DiscreteDirection _ -> False

buildDiscreteForm :: Parser.FDFL -> Discretised
buildDiscreteForm fdfl = discretisedNoMeshes
  { _discretisedMeshes = meshes'
  }
  where
  discretisedNoMeshes = Discretised
    { _discretisedMeshes = error "buildDiscreteForm: _discretisedMeshes not yet populated"
    , _discretisedLiterals = literals
    }
  meshes = catMaybes $ maybeBuildMesh <$> Map.elems (Parser.getSymbols fdfl)
  maybeBuildMesh (Parser.MeshDef meshDef) = Just $ buildMesh discretisedNoMeshes fdfl meshDef
  maybeBuildMesh _ = Nothing
  meshes' = doSpatialDiscretisation <$> scalarizeTensorFields <$> meshes
  maybeBuildLiteral (Parser.NamedLiteralDef literalDef) = Just $ buildLiteral fdfl literalDef
  maybeBuildLiteral _ = Nothing
  literals = Map.fromList . catMaybes $ maybeBuildLiteral <$> Map.elems (Parser.getSymbols fdfl)

doSpatialDiscretisation :: Mesh -> Mesh
doSpatialDiscretisation = updateMesh
  where
  updateMesh mesh = mesh
    { _meshSolves = updateSolve mesh <$> _meshSolves mesh
    }
  updateSolve mesh solve = solve
    { _solveUpdates = updateUpdate mesh solve <$> _solveUpdates solve
    , _solveBoundaryConditions = updateBoundaryCondition mesh solve <$> _solveBoundaryConditions solve
    }
  updateUpdate mesh solve update = update { _updateRHSDiscrete = buildRHSDiscrete mesh solve update }
  updateBoundaryCondition _mesh _solve bc = bc { _bcRHSDiscrete = discretiseMeshConstantExpr <$> _bcRHS bc }

constantFoldDiscretised :: Discretised -> Discretised
constantFoldDiscretised d = d
  { _discretisedMeshes = updateMesh <$> _discretisedMeshes d
  }
  where
  updateMesh mesh = mesh
    { _meshSolves = updateSolve mesh <$> _meshSolves mesh
    , _meshDimensions = substDiscreteTerminals <$> _meshDimensions mesh
    , _meshGridSpacing = substDiscreteTerminals <$> _meshGridSpacing mesh
    , _meshInitialValues  = (\(a, b) -> (a, substTerminals <$> b)) <$> _meshInitialValues mesh
    }
  updateSolve mesh solve = solve
    { _solveUpdates = updateUpdate mesh solve <$> _solveUpdates solve
    , _solveBoundaryConditions = updateBC mesh solve <$> _solveBoundaryConditions solve
    , _solveDeltaT = substDiscreteTerminals $ _solveDeltaT solve
    }
  updateUpdate _mesh _solve update = update
    { _updateRHS = substTerminals <$> _updateRHS update
    , _updateRHSDiscrete = substDiscreteTerminals <$> _updateRHSDiscrete update
    }
  updateBC _mesh _solve bc = bc
    { _bcRHS = substTerminals <$> _bcRHS bc
    , _bcRHSDiscrete = substDiscreteTerminals <$> _bcRHSDiscrete bc
    }
  substTerminals = substLiterals (_discretisedLiterals d) (\name -> ConstantRef name [])
  substDiscreteTerminals = substLiterals (_discretisedLiterals d) (\name -> ConstantDataRef name [])
  substLiterals :: Ord e => (Map String Rational) -> (String -> e) -> Expression e -> Expression e
  substLiterals values constructor = expandSymbols expandSymbol
    where
    expandSymbol s = case Map.lookup s mappedSymbols of
      Just value -> ConstantRational value
      Nothing -> Symbol s
    mappedSymbols = Map.mapKeys constructor values

scalarizeTensorFields :: Mesh -> Mesh
scalarizeTensorFields m = updateMesh m
  where
  dim = _meshDimension m
  genFlattenedName name index = concat $ intersperse "_" (name:(show <$> index))
  flattenTerminal terminal = case terminal of
    FieldRef name index -> FieldRef (genFlattenedName name index) []
    ConstantRef name index -> ConstantRef (genFlattenedName name index) []
    Direction _ -> terminal
  genIndices prefix rank = [genSuffix idx | idx <- [0..count-1]]
    where
    shape = Tensor.constructShape dim (rank - genericLength prefix)
    count = Tensor.numEntries shape
    genSuffix idx = prefix ++ Tensor.unflattenIndex shape idx
  updateMesh mesh = mesh
    { _meshFields = updateField mesh `concatMap` _meshFields mesh
    , _meshSolves = updateSolve mesh <$> _meshSolves mesh
    , _meshInitialValues = updateInitial mesh `concatMap` _meshInitialValues mesh
    }
  updateField _mesh field = [genScalarField field suffix staggering | (staggering, suffix) <- zip (_fieldStaggerSpatial field) suffixes]
    where
    rank = _fieldRank field
    suffixes = genIndices [] rank
    genScalarField f suffix staggering = f
      { _fieldName = genFlattenedName (_fieldName field) suffix
      , _fieldStaggerSpatial = [staggering]
      , _fieldRank = 0
      }
  updateInitial mesh (fieldName, expr) = genUpdate <$> suffixes
    where
    field = meshGetField mesh fieldName
    rank = _fieldRank field
    suffixes = genIndices [] rank
    genUpdate suffix = (genFlattenedName (_fieldName field) suffix, Tensor.generateTensor dim 0 (const rhs'))
      where
      rhs = Tensor.getElement expr suffix
      rhs' = substSymbols flattenTerminal rhs
  updateSolve mesh solve = solve
    { _solveUpdates = updateUpdate mesh solve `concatMap` _solveUpdates solve
    , _solveBoundaryConditions = updateBC mesh solve `concatMap` _solveBoundaryConditions solve
    }
  updateUpdate mesh _solve update = [genScalarUpdate update index | index <- suffixes]
    where
    FieldTemporalDerivative (FieldLValue fieldName idx) degree = _updateLHS update
    field = meshGetField mesh fieldName
    rank = _fieldRank field
    rhsRank = rank - genericLength idx
    suffixes = genIndices idx rank
    genScalarUpdate u index = u
      { _updateLHS = FieldTemporalDerivative (FieldLValue (genFlattenedName fieldName index) []) degree
      , _updateRHS = Tensor.generateTensor dim 0 (const rhsValue')
      }
      where
      rhsIndex = genericDrop (rank - rhsRank) index
      rhsValue = Tensor.getElement (_updateRHS u) rhsIndex
      rhsValue' = substSymbols flattenTerminal rhsValue
  updateBC mesh _solve bc = [genScalarBC index | index <- suffixes]
    where
    (FieldLValue fieldName idx) = _bcField bc
    field = meshGetField mesh fieldName
    rank = _fieldRank field
    rhsRank = rank - genericLength idx
    suffixes = genIndices idx rank
    genScalarBC index = bc
      { _bcField = FieldLValue (genFlattenedName fieldName index) []
      , _bcRHS = Tensor.generateTensor dim 0 (const rhsValue)
      }
      where
      rhsIndex = genericDrop (rank - rhsRank) index
      rhsValue = Tensor.getElement (_bcRHS bc) rhsIndex

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

buildMesh :: Discretised -> Parser.FDFL -> Parser.Mesh -> Mesh
buildMesh _ fdfl parserMesh = mesh
  where
  meshNoFields = Mesh
    { _meshName = Parser.stringLiteralValue $ Parser._meshName parserMesh
    , _meshDimension = dimension
    , _meshFields = error "buildMesh: fields not yet populated"
    , _meshSolves = error "buildMesh: solves not yet populated"
    , _meshGridSpacing = buildPerMeshDimensionScalar $ Parser._meshGridSpacing parserMesh
    , _meshDimensions = buildPerMeshDimensionScalar $ Parser._meshGridDimensions parserMesh
    , _meshInitialValues = error "buildMesh: initial fields not yet populated"
    }
  meshFields = (buildField meshNoFields fdfl . Parser.getFieldDef fdfl) <$> (Parser._meshFields parserMesh)
  meshNoInitials = meshNoFields
    { _meshFields = meshFields
    }
  meshInitialValues = (buildInitialValue meshNoInitials fdfl) <$> (Parser._meshInitialValues parserMesh)
  meshNoSolves = meshNoInitials
    { _meshInitialValues = meshInitialValues
    }
  meshSolves = (buildSolve meshNoSolves fdfl . getSolveDef) <$> (Parser._meshSolves parserMesh)
  mesh = meshNoSolves
    { _meshSolves = meshSolves
    }
  dimension = Parser._meshDimension parserMesh
  getSolveDef ident = case Parser.getSymbol fdfl ident of
    Just (Parser.SolveDef solve) -> solve
    _ -> error $ "buildMesh: unknown solve " ++ Parser.identifierValue ident
  buildPerMeshDimensionScalar parsedExpressions = expressions'
    where
    expressions = discretiseMeshConstantExpr <$> Tensor.asScalar . buildTensorRValue meshNoFields fdfl <$> parsedExpressions
    expressions' = if genericLength expressions == dimension
      then validateExpression <$> expressions
      else error $ "Incorrect number of expressions given for " ++ show dimension ++ " dimensional mesh"
    validateExpression expr = case isMeshConstant expr of
      True -> expr
      False -> error $ "Expression is not constant across grid: " ++ show expr

buildLiteral :: Parser.FDFL -> Parser.NamedLiteral -> (String, Rational)
buildLiteral _fdfl namedLiteral = (name, value)
  where
  name = Parser.stringLiteralValue $ Parser._namedLiteralName namedLiteral
  value = Parser._namedLiteralValue namedLiteral

buildField :: Mesh -> Parser.FDFL -> Parser.Field -> Field
buildField mesh _ field = Field
  { _fieldName = name
  , _fieldRank = rank
  , _fieldSymmetric = Parser._fieldSymmetric field
  , _fieldStaggerSpatial = staggering
  , _fieldStaggerTemporal = False -- TODO: implement me
  }
  where
  name = Parser.stringLiteralValue $ Parser._fieldName field
  rank = Parser._fieldRank field
  dimension = _meshDimension mesh
  numElements = dimension ^ rank
  staggering = case Parser._fieldStaggerStrategySpatial field of
    Parser.None -> genericReplicate numElements $ genericReplicate dimension False
    Parser.All -> genericReplicate numElements $ genericReplicate dimension True
    Parser.Dimension -> case rank of
      1 -> [genericReplicate n False ++ [True] ++ genericReplicate (dimension-1-n) False | n <- [0..dimension-1]]
      _ -> error $ "Dimension staggering strategy is only defined for rank 1 tensors (" ++ name ++ ")."

buildInitialValue :: Mesh -> Parser.FDFL -> (Parser.StringLiteral, Parser.FieldExpr Parser.Identifier) -> (String, Tensor (Expression Terminal))
buildInitialValue mesh fdfl (pName, pExpr) = (fieldName, fieldExpr)
  where
  fieldName = Parser.stringLiteralValue pName
  fieldExpr = buildTensorRValue mesh fdfl pExpr

meshGetField :: Mesh -> String -> Field
meshGetField mesh name = case filter (\f -> _fieldName f == name) (_meshFields mesh) of
  [] -> error $ "Unknown field " ++ name
  [f] -> f
  _ -> error $ "Multiple definitions of " ++ name

meshGetInitialValue :: Mesh -> String -> Maybe (Tensor (Expression Terminal))
meshGetInitialValue mesh fieldName =
  case filter (\(name, _) -> fieldName == name) (_meshInitialValues mesh) of
    [] -> Nothing
    ((_, expr):_) -> Just expr

discretiseMeshConstantExpr :: Expression Terminal -> Expression DiscreteTerminal
discretiseMeshConstantExpr = substSymbols makeDiscreteConstant
  where
  makeDiscreteConstant (ConstantRef name idx) = ConstantDataRef name idx
  makeDiscreteConstant sym = error $ "Unable to convert symbol " ++ show sym ++ " to mesh cell-independent value"

buildBoundaryCondition :: Mesh -> Parser.FDFL -> Parser.Solve -> Parser.BoundaryCondition -> BoundaryCondition
buildBoundaryCondition mesh fdfl _solve bc = BoundaryCondition
  { _bcType = bcType
  , _bcField = bcField
  , _bcRHS = buildTensorRValue mesh fdfl $ Parser._bcRHS bc
  , _bcRHSDiscrete = error "buildBoundaryCondition: spatial discretisation not yet applied"
  , _bcSubdomains = bcSubdomains
  }
  where
  (bcType, bcField) = buildLHS $ Parser._bcLHS bc
  buildLHS (Parser.FieldNormalDerivative lValue) = (Neumann, findLValue mesh fdfl lValue)
  buildLHS lValue = (Dirichlet, findLValue mesh fdfl lValue)
  bcSubdomains = case Parser._bcSubdomains bc of
    Nothing -> [AllExteriorEdges]
    Just domains -> (TaggedEdgeString . Parser.stringLiteralValue) <$> domains

buildSolve :: Mesh -> Parser.FDFL -> Parser.Solve -> Solve
buildSolve mesh fdfl solve = Solve
  { _solveName = Parser.stringLiteralValue $ Parser._solveName solve
  , _solveSpatialOrder = Parser._solveSpatialOrder solve
  , _solveTemporalOrder = temporalOrder
  , _solveUpdates = (buildUpdate mesh fdfl solve . getExpressionDef) <$> (Parser._solveEquations solve)
  , _solveBoundaryConditions = (buildBoundaryCondition mesh fdfl solve . getBCDef) <$> (Parser._solveBoundaryConditions solve)
  , _solveDeltaT = buildDeltaT
  }
  where
  temporalOrder = Parser._solveTemporalOrder solve
  getExpressionDef ident = case Parser.getSymbol fdfl ident of
    Just (Parser.EquationDef equ) -> equ
    _ -> error $ "buildSolve: unknown equation " ++ Parser.identifierValue ident
  getBCDef ident = case Parser.getSymbol fdfl ident of
    Just (Parser.BoundaryConditionDef bc) -> bc
    _ -> error $ "buildSolve: unknown boundary condition " ++ Parser.identifierValue ident
  buildDeltaT = discretiseMeshConstantExpr <$> Tensor.asScalar . buildTensorRValue mesh fdfl $ Parser._solveDeltaT solve

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
    FieldRef name tensorIndex -> SemiDiscreteFieldRef name tensorIndex (genericReplicate dimension 0)
    Direction i -> SemiDiscreteDirection i
  collapseDerivative e = case e of
    Diff (Application (ApplyUserDefined (SemiDiscreteFieldRef name tensorIndex derivatives) dims)) (SemiDiscreteDirection d) n ->
      Application $ ApplyUserDefined (SemiDiscreteFieldRef name tensorIndex derivatives') dims
      where
      derivatives' = (genericTake d derivatives) ++ [n + genericIndex derivatives d] ++ (genericDrop (d + 1) derivatives)
    _ -> e
  removeFunctions e = case e of
    Application (ApplyUserDefined sym@(SemiDiscreteFieldRef _ _ _) _) -> Symbol sym
    _ -> e

buildRHSDiscrete :: Mesh -> Solve ->
                    Update -> Tensor(Expression DiscreteTerminal)
buildRHSDiscrete mesh solve update = rhs
  where
  lhs = _updateLHS update
  rhsContinuous = _updateRHS update
  dimension = _meshDimension mesh
  order = _solveSpatialOrder solve
  shape = Tensor.getShape rhsContinuous
  rhs = Tensor.mapWithIndex makeDiscrete rhsContinuous
  lhsFieldName = case lhs of
    FieldTemporalDerivative (FieldLValue name _) _ -> name
  makeDiscrete :: TensorIndex -> Expression Terminal -> Expression DiscreteTerminal
  makeDiscrete idx expr = discrete
    where
    lhsStaggering = (_fieldStaggerSpatial $ meshGetField mesh lhsFieldName) `genericIndex` (Tensor.flattenIndex shape idx)
    semiDiscrete = makeSemiDiscrete dimension expr
    discrete = expandSymbols expandStencilSymbol semiDiscrete
    expandStencilSymbol :: SemiDiscreteTerminal -> Expression DiscreteTerminal
    expandStencilSymbol sym = case sym of
      SemiDiscreteConstantRef name tensorIndex -> Symbol $ ConstantDataRef name tensorIndex
      SemiDiscreteFieldRef name tensorIndex derivatives -> stencilExpression
        where
        field = meshGetField mesh name
        fieldRank = _fieldRank field
        fieldShape = Tensor.constructShape dimension fieldRank
        fieldStaggering = (_fieldStaggerSpatial field) `genericIndex` (Tensor.flattenIndex fieldShape tensorIndex)
        relativeStaggering = computeStaggering lhsStaggering fieldStaggering
        stencilExpression = scaleFactor * (foldl (+) 0 terms)
        terms = buildTerm <$> Map.toList (_stencilValues stencil)
        scaleFactor = foldl (*) (fromInteger 1) (zipWith (^^) (_meshGridSpacing mesh) (_stencilScalingPowers stencil))
        buildTerm (index, coeff) = (fromRational coeff) * (Symbol $ FieldDataRef name tensorIndex index)
        stencilSpec = StencilSpec
          { _stencilSpecOrder = order
          , _stencilSpecDerivatives = derivatives
          , _stencilSpecStaggering = relativeStaggering
          }
        stencil = buildStencil stencilSpec
      SemiDiscreteDirection i -> ((Symbol $ DiscreteDirection i) + staggerOffset) * ((_meshGridSpacing mesh) `genericIndex` i)
          where
          staggerOffset = if genericIndex lhsStaggering i then 0.5 else 0.0

computeStaggering :: [Bool] -> [Bool] -> [Stagger]
computeStaggering first second = assert (length first == length second) zipWith computeStagger first second
  where
  computeStagger f s = case (f, s) of
    (False, True) -> StaggerPos
    (True, False) -> StaggerNeg
    _ -> StaggerNone

genNabla :: Integer -> Tensor (Expression Terminal -> Expression Terminal)
genNabla dim = Tensor.constructTensor dim 1 [diff $ Direction dir | dir <- [0 .. dim-1]]

buildTensorRValue :: Mesh -> Parser.FDFL -> Parser.FieldExpr Parser.Identifier -> Tensor (Expression Terminal)
buildTensorRValue mesh fdfl expr = case expr of
  Parser.FieldTemporalDerivative _ -> error "buildUpdate: Temporal derivative not expected in RHS"
  Parser.FieldNormalDerivative _ -> error "buildUpdate: Normal derivative not expected in RHS"
  Parser.FieldAddition l r -> Tensor.add (buildRValue l) (buildRValue r)
  Parser.FieldInner l r -> Tensor.inner (buildRValue l) (buildRValue r)
  Parser.FieldOuter l r -> Tensor.outer (buildRValue l) (buildRValue r)
  Parser.FieldDot l r -> Tensor.dot (buildRValue l) (buildRValue r)
  Parser.FieldDivision l r -> Tensor.divide (buildRValue l) (buildRValue r)
  Parser.FieldGradient t -> Tensor.outerWithOp ($) nabla (buildRValue t)
  Parser.FieldDivergence t -> Tensor.dotWithOp ($) (+) nabla (buildRValue t)
  Parser.FieldSpatialDerivative t i -> genDerivative i <$> (buildRValue t)
  Parser.FieldIndexOperation i t -> genScalar $ Tensor.getElement (buildRValue t) i
  Parser.FieldLiteral literal -> case literal of
    Parser.ScalarConstant r -> Tensor.constructTensor dimension 0 [ConstantRational r]
    Parser.PermutationSymbol -> genLC dimension
  Parser.FieldRef ref -> case Parser.getSymbol fdfl ref of
    Just (Parser.MeshConstantDef constant) ->
      buildAccessTensor (Parser._meshConstantName constant) (Parser._meshConstantRank constant) constructor
        where
        constructor name = Symbol . ConstantRef name
    Just (Parser.FieldDef field) ->
      buildAccessTensor (Parser._fieldName field) (Parser._fieldRank field) constructor
        where
        constructor name index = Application $ ApplyUserDefined (FieldRef name index) directions
    Just (Parser.FieldExprDef def) -> buildRValue def
    Just (Parser.NamedLiteralDef def) -> genScalar . Symbol $ ConstantRef (Parser.stringLiteralValue $ Parser._namedLiteralName def) []
    Just _ -> error $ "buildTensorRValue: unable to treat symbol as field: " ++ Parser.identifierValue ref
    Nothing -> error $ "buildTensorRValue: unknown symbol " ++ Parser.identifierValue ref
  Parser.FieldTensorElements elements -> Tensor.fromSubTensors $ buildRValue <$> elements
  Parser.FieldPower a b -> genScalar $ a' ** b'
    where
    a' = Tensor.asScalar $ buildRValue a
    b' = Tensor.asScalar $ buildRValue b
  Parser.FieldExponent a -> genScalar $ exp (Tensor.asScalar $ buildRValue a)
  Parser.FieldSin a -> genScalar $ sin (Tensor.asScalar $ buildRValue a)
  Parser.FieldCos a -> genScalar $ cos (Tensor.asScalar $ buildRValue a)
  Parser.FieldTan a -> genScalar $ tan (Tensor.asScalar $ buildRValue a)
  Parser.FieldPi -> genScalar pi
  Parser.FieldPosition -> position
  where
  dimension = _meshDimension mesh
  nabla = genNabla dimension
  position = Tensor.constructTensor dimension 1 $ Symbol . Direction <$> [0..dimension - 1]
  buildRValue = buildTensorRValue mesh fdfl
  genScalar s = Tensor.constructTensor dimension 0 [s]
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

findLValue :: Mesh -> Parser.FDFL -> Parser.FieldExpr Parser.Identifier -> FieldLValue
findLValue mesh fdfl expr = case expr of
  Parser.FieldRef ident -> FieldLValue (getFieldName ident) []
  Parser.FieldIndexOperation indices (Parser.FieldRef ident) -> if genericLength indices == getFieldRank ident
    then FieldLValue (getFieldName ident) indices
    else error $ "All indices must be specified in tensor assignment: " ++ show expr
  _ -> error $ "Unhandled L-value expression type for field assignment: " ++ show expr
  where
  getFieldName = Parser.stringLiteralValue . Parser._fieldName . Parser.getFieldDef fdfl
  getFieldRank = _fieldRank . meshGetField mesh . getFieldName

maxTimestepOrder :: Update -> Integer
maxTimestepOrder update = Map.foldrWithKey (\k _ b -> max k b) 0 (_updateTimeSteppingSchemes update)

getTimestepping :: Update -> Integer -> Maybe (Expression TemporalTerminal)
getTimestepping update order = Map.lookup order (_updateTimeSteppingSchemes update)

numPreviousTimestepsNeeded :: Update -> Integer -> Integer
numPreviousTimestepsNeeded update order = foldl max 0 unknownsDerivatives
  where
  unknowns = case Map.lookup order (_updateTimeSteppingSchemes update) of
    Just expr -> Set.toList $ vars expr
    Nothing -> error $ "numPreviousTimestepsNeeded: missing expression for order-" ++ show order ++ " timestepping."
  unknownsDerivatives = catMaybes $ derivativeOff <$> unknowns
  derivativeOff sym = case sym of
    PreviousDerivative n -> Just n
    _ -> Nothing

findFieldUpdate :: FieldLValue -> Solve -> Update
findFieldUpdate lhs solve = case matchingUpdates of
  [update] -> update
  _ -> error $ "findFieldUpdateRHS: Unable to find update for " ++ show lhs
  where
  updates = _solveUpdates solve
  matchingUpdates = filter (\u -> getLValue u == lhs) updates

buildUpdate :: Mesh -> Parser.FDFL -> Parser.Solve -> Parser.Equation -> Update
buildUpdate mesh fdfl solve equ = Update
  { _updateLHS = lhs
  , _updateRHS = rhs
  , _updateRHSDiscrete = error "buildUpdate: spatial discretisation not yet applied"
  , _updateTimeSteppingSchemes = timestepping
  }
  where
  lhs = buildLHS $ Parser._fieldUpdateLHS equ
  rhs = buildTensorRValue mesh fdfl $ Parser._fieldUpdateRHS equ
  temporalOrder = Parser._solveTemporalOrder solve
  timestepping = case lhs of
    FieldTemporalDerivative _ d -> Map.fromList [(n, buildAdamsBashForth d n) | n <- [1..temporalOrder]]
  buildLHS expr = buildDerivative 0 expr
    where
    buildDerivative n (Parser.FieldTemporalDerivative subExpr) = buildDerivative (n + 1) subExpr
    buildDerivative n lValue = FieldTemporalDerivative (findLValue mesh fdfl lValue) n

mergeBoundingMap :: Map FieldLValue [(Integer, Integer)] -> Map FieldLValue [(Integer, Integer)] -> Map FieldLValue [(Integer, Integer)]
mergeBoundingMap a b = foldr (uncurry $ Map.insertWith mergeBoundingRange) a (Map.toList b)

expressionGetGhostSizes :: Expression DiscreteTerminal -> Map FieldLValue [(Integer, Integer)]
expressionGetGhostSizes expr = result
  where
  result = foldr (uncurry $ Map.insertWith mergeBoundingRange) Map.empty accesses
  unknowns = vars expr
  accesses = catMaybes $ getGhost <$> Set.toList unknowns
  getGhost v = case v of
    FieldDataRef name tensorIndex stencilIndex -> Just (FieldLValue name tensorIndex, makeRange <$> stencilIndex)
    ConstantDataRef _ _ -> Nothing
    DiscreteDirection _ -> Nothing
  makeRange i = (i, i)

updateGetGhostSizes :: Update -> Map FieldLValue [(Integer, Integer)]
updateGetGhostSizes update = foldr mergeBoundingMap Map.empty (expressionGetGhostSizes <$> tensorEntries)
  where
  rhs = _updateRHSDiscrete update
  tensorEntries = Tensor.getEntries rhs

solveGetGhostSizes :: Solve -> Map FieldLValue [(Integer, Integer)]
solveGetGhostSizes solve = foldr mergeBoundingMap Map.empty (updateGetGhostSizes <$> _solveUpdates solve)
