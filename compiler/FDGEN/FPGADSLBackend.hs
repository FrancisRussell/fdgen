module FDGEN.FPGADSLBackend (FPGADSLBackend(..)) where
import FDGEN.Backend(Backend(..))
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import FDGEN.Discrete ( Discretised(..), Mesh(..), Field(..), Solve(..)
                      , FieldLValue(..), BoundaryCondition(..), findFieldUpdate
                      , numPreviousTimestepsNeeded, DiscreteTerminal(..)
                      , Update(..), constantFoldDiscretised, maxTimestepOrder
                      , getTimestepping, TemporalTerminal(..), solveGetGhostSizes
                      , meshGetField, BoundaryConditionType(..))
import FDGEN.Algebra (Expression(..), PairSeq(..), expandSymbols)
import Control.Applicative ((<$>))
import Data.Ratio (numerator, denominator)
import FDGEN.Precedence ( PDoc(..), pDoc, renderInfix, renderTerminal
                        , Precedence(..), Assoc(..), renderPrefix
                        , renderPrefixMultiParam, makeAtomic)
import FDGEN.Pretty (PrettyPrintable(..))
import FDGEN.Util (mergeBoundingRange)
import Data.Map (Map)
import Data.List (genericReplicate, genericIndex)
import qualified FDGEN.Template as Template
import qualified FDGEN.Tensor as Tensor
import qualified FDGEN.Discrete as Discrete
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data FPGADSLBackend = FPGADSLBackend

data Context = Context
  { _contextCellVariables :: [CellVariable]
  , _contextMeshDimensions :: [DSLExpr]
  , _contextBCDirectives :: [BCDirective]
  } deriving Show

data CellVariable = CellVariable
  { _cellVariableName :: String
  , _cellVariableExpr :: DSLExpr
  , _cellVariableInitialExpr :: Maybe (Integer, DSLExpr)
  } deriving Show

data BCDirective = BCDirective
  { _bcDirectiveVariable :: String
  , _bcDirectiveAxis :: Axis
  , _bcDirectiveOffset :: DSLExpr
  , _bcDirectiveLow :: DSLExpr
  , _bcDirectiveHigh :: DSLExpr
  , _bcDirectiveAction :: ValueSource
  } deriving Show

data Sign
  = Negative
  | Positive
  deriving (Eq, Ord, Show)

data EdgeDomain
  = LeftEdge
  | RightEdge
  | TopEdge
  | BottomEdge
  deriving (Eq, Ord, Show)

data Axis
  = Vertical
  | Horizontal
  deriving (Eq, Ord, Show)

data MeshInfo = MeshInfo
  { _meshInfoMargins :: [(Integer, Integer)]
  , _meshInfoDimensions :: [DSLExpr]
  } deriving Show

data ValueSource
  = CopyValueTowardsZero
  | CopyValueAwayFromZero
  | NegateValueTowardsZero
  | NegateValueAwayFromZero
  | SetValue DSLExpr
  deriving Show


class PDocPrintable a where
  pDocPrint :: a -> PDoc

instance PDocPrintable PDoc where
  pDocPrint = id

instance PDocPrintable ValueSource where
  pDocPrint s = case s of
    CopyValueTowardsZero -> buildAtom s
    CopyValueAwayFromZero -> buildAtom s
    NegateValueTowardsZero -> buildAtom s
    NegateValueAwayFromZero -> buildAtom s
    SetValue e -> renderApplication "SetValue" [pDocPrint e]
    where
      buildAtom t = PDoc (toDoc $ show t) NoAssoc AtomPrec

getEdgeDomainAxis :: EdgeDomain -> Axis
getEdgeDomainAxis d = case d of
  LeftEdge -> Vertical
  RightEdge -> Vertical
  TopEdge -> Horizontal
  BottomEdge -> Horizontal

axisDimension :: Axis -> Integer
axisDimension Horizontal = 0
axisDimension Vertical = 1

normalSign :: EdgeDomain -> Sign
normalSign d = case d of
  LeftEdge -> Negative
  RightEdge -> Positive
  TopEdge -> Positive
  BottomEdge -> Negative

allExteriorEdgeDomains :: [EdgeDomain]
allExteriorEdgeDomains = [LeftEdge, RightEdge, TopEdge, BottomEdge]

tagToEdgeDomains :: String -> [EdgeDomain]
tagToEdgeDomains tag = case tag of
  "left_edge" -> [LeftEdge]
  "right_edge" -> [RightEdge]
  "top_edge" -> [TopEdge]
  "bottom_edge" -> [BottomEdge]
  _ -> error $ "Unrecognised tag for edge domain " ++ show tag

translateEdgeDomain :: Discrete.EdgeDomain -> [EdgeDomain]
translateEdgeDomain d = case d of
  Discrete.AllExteriorEdges -> allExteriorEdgeDomains
  Discrete.TaggedEdgeString s -> tagToEdgeDomains s

concatMapUniq :: Ord b => (a -> [b]) -> [a] -> [b]
concatMapUniq f = Set.toList . Set.fromList . concatMap f

renderApplication :: String -> [PDoc] -> PDoc
renderApplication name params = renderPrefixMultiParam (name, PrecLevel 10) params

renderDSLExpr :: PDocPrintable a => a -> String
renderDSLExpr = prettyPrint . pDocPrint

instance PDocPrintable DSLExpr where
  pDocPrint = renderDSLExpr'
    where
    renderDSLExpr' expr = case expr of
      DSLAdd a b -> renderInfix (" + ", PrecLevel 6, LeftAssoc) (renderDSLExpr' a) (renderDSLExpr' b)
      DSLSub a b -> renderInfix (" - ", PrecLevel 6, LeftAssoc) (renderDSLExpr' a) (renderDSLExpr' b)
      DSLMult a b -> renderInfix (" * ", PrecLevel 7, LeftAssoc) (renderDSLExpr' a) (renderDSLExpr' b)
      DSLDiv a b -> renderInfix (" / ", PrecLevel 7, LeftAssoc) (renderDSLExpr' a) (renderDSLExpr' b)
      DSLNegate  e -> renderPrefix ("-", PrecLevel 6) (renderDSLExpr' e)
      DSLIntPower e i -> renderApplication "IntPower" [renderDSLExpr' e, renderNum i]
      DSLCellVariable name -> renderTerminal name
      DSLConstant name e -> renderApplication "Constant" [renderTerminal name, renderDSLExpr' e]
      DSLInt i -> renderApplication "EInt" [renderNum i]
      DSLDouble d -> renderNum d
      DSLOffset e x y -> renderApplication "Offset" [renderDSLExpr' e, renderNum x, renderNum y]
      DSLCurrent e -> renderApplication "Current" [renderDSLExpr' e]
      DSLCos e -> renderApplication "cos" [renderDSLExpr' e]
      DSLSin e -> renderApplication "sin" [renderDSLExpr' e]
    renderNum :: (Show a, Eq a, Num a) => a -> PDoc
    renderNum n = if signum n == fromInteger (-1)
      then renderPrefix ("-", PrecLevel 6) (renderTerminal . show $ abs n)
      else renderTerminal $ show n

data DSLExpr
  = DSLAdd DSLExpr DSLExpr
  | DSLSub DSLExpr DSLExpr
  | DSLMult DSLExpr DSLExpr
  | DSLDiv DSLExpr DSLExpr
  | DSLNegate DSLExpr
  | DSLIntPower DSLExpr Int
  | DSLCellVariable String
  | DSLConstant String DSLExpr
  | DSLInt Int
  | DSLDouble Double
  | DSLOffset DSLExpr Int Int
  | DSLCurrent DSLExpr
  | DSLCos DSLExpr
  | DSLSin DSLExpr
  deriving (Eq, Ord, Show)

instance Num DSLExpr where
  (+) = DSLAdd
  (*) = DSLMult
  (-) = DSLSub
  abs = error "abs undefined for DSLExpr"
  signum = error "signum undefined for DSLExpr"
  fromInteger = DSLInt . fromIntegral
  negate = DSLNegate

instance Fractional DSLExpr where
  (/) = DSLDiv
  fromRational = DSLDouble . fromRational

getSingleton :: String -> [e] -> e
getSingleton _ [e] = e
getSingleton name lst = error $ "Expected single " ++ name ++ ", but there were " ++ (show $ length lst)

mergeGhostSizes :: Integer -> Map FieldLValue [(Integer, Integer)] -> [(Integer, Integer)]
mergeGhostSizes dim m = Map.foldr mergeBoundingRange (genericReplicate dim (0, 0)) m

generateContext :: Discretised -> Context
generateContext discretised = context
  where
  context = Context
    { _contextCellVariables = buildCellVariables discretised mesh solve
    , _contextBCDirectives = buildBCDirectives discretised mesh solve
    , _contextMeshDimensions = oversizedDimensionsDSL
    }
  meshInfo = MeshInfo
    { _meshInfoMargins = margins
    , _meshInfoDimensions = meshDimensionsDSL
    }
  meshDimensionsDSL = buildDSLExprInteger expandDiscreteTerminal <$> meshDimensions
  ghostSizes = mergeGhostSizes meshDimension $ solveGetGhostSizes solve
  margins = (\(a,b) -> (abs a, abs b)) <$> ghostSizes
  oversizedDimensionsDSL = (\(a, (l, u)) -> a + fromInteger (l + u + 1)) <$> zip meshDimensionsDSL margins
  mesh = getSingleton "mesh" (_discretisedMeshes discretised)
  solve = getSingleton "solve" (_meshSolves mesh)
  meshDimensions = _meshDimensions mesh
  meshDimension = _meshDimension mesh

buildCellVariables :: Discretised -> Mesh -> Solve -> [CellVariable]
buildCellVariables discretised mesh solve =
  concatMap (fieldToCellVariables discretised mesh solve) (_meshFields mesh)

buildBCDirectives :: Discretised -> Mesh -> Solve -> [BCDirective]
buildBCDirectives discretised mesh solve =
  concatMap (bcToDirectives discretised mesh solve) (_solveBoundaryConditions solve)

bcToDirectives :: Discretised -> Mesh -> Solve -> BoundaryCondition -> [BCDirective]
bcToDirectives _discretised mesh solve bc = buildDirective <$> edge_domains
  where
  edge_domains = concatMapUniq translateEdgeDomain (_bcSubdomains bc)
  buildDirective edge_domain = BCDirective
    { _bcDirectiveVariable = fieldName
    , _bcDirectiveAxis = getEdgeDomainAxis edge_domain
    , _bcDirectiveOffset = edgeOffset
    , _bcDirectiveLow = fst planeLimits
    , _bcDirectiveHigh = snd planeLimits
    , _bcDirectiveAction = action
    }
    where
      bcAxis = getEdgeDomainAxis edge_domain
      bcDimension = axisDimension bcAxis
      bcValue = buildDSLExpr expandDiscreteTerminal . Tensor.asScalar $ _bcRHSDiscrete bc
      fieldLValue = _bcField bc
      fieldName = getScalarFieldName fieldLValue
      bcType = _bcType bc
      meshDimension = _meshDimension mesh
      meshDimensions = _meshDimensions mesh
      makeIntegerExpr = buildDSLExprInteger expandDiscreteTerminal
      ghostSizes = mergeGhostSizes meshDimension $ solveGetGhostSizes solve
      margins = (\(a,b) -> (abs a, abs b)) <$> ghostSizes
      field = meshGetField mesh fieldName
      fieldStaggered = genericIndex (getSingleton "stagger" $ _fieldStaggerSpatial field) bcDimension
      sign = normalSign edge_domain
      edgeOffset = makeIntegerExpr $ case (bcType, fieldStaggered, sign) of
        (Dirichlet, False, Negative) -> left
        (_, _, Negative) -> left - 1
        (_, _, Positive) -> right
        where
          perpDimension = case bcAxis of
            Horizontal -> 1
            Vertical -> 0
          left = fromIntegral . fst $ genericIndex margins perpDimension
          right = left +  genericIndex meshDimensions perpDimension
      action = case (bcType, fieldStaggered, sign) of
        (Dirichlet, False, _) -> SetValue bcValue
        (Dirichlet, True, Negative) -> NegateValueAwayFromZero
        (Dirichlet, True, Positive) -> NegateValueTowardsZero
        (Neumann, _, Negative) -> CopyValueAwayFromZero
        (Neumann, _, Positive) -> CopyValueTowardsZero
      planeLimits = (makeIntegerExpr $ fromIntegral marginLow, makeIntegerExpr $ meshWidth + fromIntegral marginLow)
        where
          (marginLow, _marginHigh) = genericIndex margins bcDimension
          meshWidth = genericIndex meshDimensions bcDimension
          limits dim = (makeIntegerExpr $ fromIntegral marginLow, makeIntegerExpr $ meshWidth + fromIntegral marginLow)

getScalarFieldName :: FieldLValue -> String
getScalarFieldName lvalue = case lvalue of
  FieldLValue name [] -> name
  FieldLValue name _ -> error $ "Boundary condition for field " ++ name ++ " is not scalarized"

getDerivativeName :: String -> Integer -> String
getDerivativeName name d = name ++ "_dt" ++ show d

getPreviousDerivative :: String -> Integer -> DSLExpr
getPreviousDerivative name offset = case offset of
  0 -> DSLCurrent . DSLCellVariable $ getDerivativeName name 0
  _ -> DSLCellVariable $ getDerivativeName name (offset - 1)

generateTimestepping :: Solve -> Update -> String -> Integer -> DSLExpr
generateTimestepping solve update name order = buildDSLExpr translateTerminal expr
  where
  expr = case getTimestepping update order of
    Just e -> e
    Nothing -> error $ "generateTimestepping: missing expression for order " ++ show order
  translateTerminal t = case t of
    PreviousValue -> Symbol $ DSLCellVariable name
    DeltaT -> expandSymbols expandDiscreteTerminal $ _solveDeltaT solve
    PreviousDerivative i -> Symbol $ getPreviousDerivative name i

fieldToCellVariables :: Discretised -> Mesh -> Solve -> Field -> [CellVariable]
fieldToCellVariables _discretised _mesh solve field = (cellVariable:cellVariableDerivatives)
  where
  update = findFieldUpdate (FieldLValue (_fieldName field) []) solve
  rhsTensor = _updateRHSDiscrete update
  rhs = Tensor.asScalar rhsTensor
  maxTemporalOrder = maxTimestepOrder update
  -- For Euler updating, we do not need to know any previous derivatives, but since we don't incorporate
  -- the derivative directly into the update expression we need to allocate an (unused) derivative.
  numDerivativesNeeded = numPreviousTimestepsNeeded update maxTemporalOrder
  numDerivativesStored = max numDerivativesNeeded 1
  name = _fieldName field
  cellVariableDerivatives = [cellVariableDerivative n | n <- [0..numDerivativesStored-1]]
  cellVariable = CellVariable
    { _cellVariableName = name
    , _cellVariableExpr = generateTimestepping solve update name maxTemporalOrder
    , _cellVariableInitialExpr = if numDerivativesNeeded == 0
      then Nothing
      else Just (numDerivativesNeeded, generateTimestepping solve update name 1)
    }
  cellVariableDerivative n = CellVariable
    { _cellVariableName = getDerivativeName name n
    , _cellVariableExpr = if n == 0
      then buildDSLExpr expandDiscreteTerminal rhs
      else getPreviousDerivative name n
    , _cellVariableInitialExpr = Nothing
    }

expandDiscreteTerminal :: DiscreteTerminal -> Expression DSLExpr
expandDiscreteTerminal s = Symbol $ case s of
    FieldDataRef name [] offsets  -> case offsets of
      [0, 0] -> DSLCellVariable name
      [x, y] -> DSLOffset (DSLCellVariable name) (fromIntegral x) (fromIntegral y)
      _ -> error $ "Expected 2D stencil offset expression: " ++ show s
    ConstantDataRef _ _ -> error $ "No non-literal constants expected in FPGA backend (was constant folding applied?): " ++ show s
    FieldDataRef _ _ _  -> error $ "Expected no indices for field index expression (were tensor fields scalarized?): " ++ show s

buildDSLExprInteger :: (Ord e, Show e) => (e -> Expression DSLExpr) -> Expression e -> DSLExpr
buildDSLExprInteger translateSymbol = buildDSLExpr' . expandSymbols translateSymbol
  where
  buildDSLExpr' e = case e of
    Symbol s -> s
    Sum seq' -> buildPairSeq (0, (+)) (1, multRational) seq'
    Product seq' -> buildPairSeq (1, (*)) (1, raiseInt) seq'
    ConstantRational r -> if denominator r == 1
      then DSLInt . fromInteger $ numerator r
      else error $ "Expected integer, but found rational: " ++ show r
    ConstantFloat _ -> error $ "buildDSLExprInteger: Unexpected float in integer expression: " ++ show e
    _ -> error $ "Unexpected term when trying to contruct integer expression: " ++ show e
    where
    multRational a b = if b /= (-1)
      then a * (buildDSLExpr' $ ConstantRational b)
      else DSLNegate a
    raiseInt expr p = if denominator p == 1
      then DSLIntPower expr (fromIntegral $ numerator p)
      else error "Cannot translate non-integral power expression"
    buildPairSeq (null1, op1) (null2, op2) seq' = foldl1 op1 pairExprs'
      where
      overall = buildDSLExpr' . ConstantRational $ _psOverall seq'
      pairExprs = transformPair <$> (Map.toList $ _psTerms seq')
      pairExprs' = if _psOverall seq' == null1 && (not $ Map.null (_psTerms seq'))
        then pairExprs
        else (overall:pairExprs)
      transformPair (a, b) = if b /= null2
        then (buildDSLExpr' a) `op2` b
        else (buildDSLExpr' a)

buildDSLExpr :: (Ord e, Show e) => (e -> Expression DSLExpr) -> Expression e -> DSLExpr
buildDSLExpr translateSymbol = buildDSLExpr' . expandSymbols translateSymbol
  where
  buildDSLExpr' e = case e of
    Symbol s -> s
    Sum seq' -> buildPairSeq (0, (+)) (1, multRational) seq'
    Product seq' -> buildPairSeq (1, (*)) (1, raiseInt) seq'
    ConstantFloat f -> DSLDouble f
    ConstantRational r -> DSLDouble $ fromRational r
    Abs _ -> error $ "unhandled expression: " ++ show e
    Signum _ -> error $ "unhandled expression: " ++ show e
    Ln _ -> error $ "unhandled expression: " ++ show e
    Diff _ _ _ -> error $ "unhandled expression: " ++ show e
    Int _ _ -> error $ "unhandled expression: " ++ show e
    Function _ _ -> error $ "unhandled expression: " ++ show e
    where
    multRational a b = if b /= (-1)
      then a * (buildDSLExpr' $ ConstantRational b)
      else DSLNegate a
    raiseInt expr p = if denominator p == 1
      then DSLIntPower expr (fromIntegral $ numerator p)
      else error "Cannot translate non-integral power expression"
    buildPairSeq (null1, op1) (null2, op2) seq' = foldl1 op1 pairExprs'
      where
      overall = buildDSLExpr' . ConstantRational $ _psOverall seq'
      pairExprs = transformPair <$> (Map.toList $ _psTerms seq')
      pairExprs' = if _psOverall seq' == null1 && (not $ Map.null (_psTerms seq'))
        then pairExprs
        else (overall:pairExprs)
      transformPair (a, b) = if b /= null2
        then (buildDSLExpr' a) `op2` b
        else (buildDSLExpr' a)

buildDictionary :: Context -> Template.Dict
buildDictionary context = template'''
  where
  template = Template.insert "fields" (Template.ListVal $ Template.DictVal <$> fieldDictionaries) Template.emptyDict
  template' = Template.insert "boundary_conditions" (Template.ListVal $ Template.DictVal <$> bcDictionaries) template
  template'' = Template.insert "width" (Template.StringVal $ renderDSLExpr $ (_contextMeshDimensions context !! 0)) template'
  template''' = Template.insert "height" (Template.StringVal $ renderDSLExpr $ (_contextMeshDimensions context !! 1)) template''
  fieldDictionaries = buildCellVariableDictionary <$> _contextCellVariables context
  bcDictionaries = buildBCDirectiveDictionary <$> _contextBCDirectives context

buildCellVariableDictionary :: CellVariable -> Template.Dict
buildCellVariableDictionary cellVariable = Map.fromList $
  [ ("name", Template.StringVal name)
  , ("update", Template.StringVal . renderDSLExpr $ _cellVariableExpr cellVariable)
  ] ++ initial
  where
  name = _cellVariableName cellVariable
  initial = case _cellVariableInitialExpr cellVariable of
    Nothing -> []
    Just (count, expr) -> [("initial", initialMap)]
      where
      initialMap = Template.DictVal $ Map.fromList
        [ ("count", Template.StringVal $ show count)
        , ("expression", Template.StringVal $ renderDSLExpr expr)
        ]

buildBCDirectiveDictionary :: BCDirective -> Template.Dict
buildBCDirectiveDictionary bcDirective = Map.fromList $
  [ ("variable", Template.StringVal name)
  , ("axis", Template.StringVal . show $ _bcDirectiveAxis bcDirective)
  , ("offset", makeVal $ _bcDirectiveOffset bcDirective)
  , ("low", makeVal $ _bcDirectiveLow bcDirective)
  , ("high", makeVal $ _bcDirectiveHigh bcDirective)
  , ("action", makeVal $ _bcDirectiveAction bcDirective)
  ]
  where
  name = _bcDirectiveVariable bcDirective
  makeVal :: PDocPrintable a => a -> Template.Val
  makeVal = Template.StringVal . renderDSLExpr . makeAtomic . pDocPrint

instance Backend FPGADSLBackend
  where
  processDiscretised _ discreteForm = do
    template <- readFile "./templates/jamie_dsl.template"
    case Template.populate (buildDictionary context) template of
      Left err -> hPutStrLn stderr (show err) >> exitFailure
      Right generated -> putStrLn generated
    where
    discreteForm' = constantFoldDiscretised discreteForm
    context = generateContext discreteForm'
