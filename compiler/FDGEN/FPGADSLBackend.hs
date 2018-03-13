module FDGEN.FPGADSLBackend (FPGADSLBackend(..)) where
import FDGEN.Backend(Backend(..))
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import FDGEN.Discrete ( Discretised(..), Mesh(..), Field(..), Solve(..)
                      , FieldLValue(..), findFieldUpdate
                      , numPreviousTimestepsNeeded, DiscreteTerminal(..)
                      , Update(..), constantFoldDiscretised, maxTimestepOrder
                      , getTimestepping, TemporalTerminal(..))
import FDGEN.Algebra (Expression(..), PairSeq(..))
import Control.Applicative ((<$>))
import Data.Ratio (numerator, denominator)
import FDGEN.Precedence (PDoc, pDoc, renderInfix, renderTerminal, Precedence(..), Assoc(..), renderPrefix, renderPrefixMultiParam)
import FDGEN.Pretty (PrettyPrintable(..))
import qualified FDGEN.Template as Template
import qualified FDGEN.Tensor as Tensor
import qualified Data.Map.Strict as Map

data FPGADSLBackend = FPGADSLBackend

data Context = Context
  { _contextCellVariables :: [CellVariable]
  } deriving Show

data CellVariable = CellVariable
  { _cellVariableName :: String
  , _cellVariableExpr :: DSLExpr
  } deriving Show

renderDSLExpr :: DSLExpr -> String
renderDSLExpr = prettyPrint . pDoc . renderDSLExpr'
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
    DSLInt i -> renderNum i
    DSLDouble d -> renderNum d
    DSLOffset e x y -> renderApplication "Offset" [renderDSLExpr' e, renderNum x, renderNum y]
    DSLCurrent e -> renderApplication "Current" [renderDSLExpr' e]
    DSLCos e -> renderApplication "cos" [renderDSLExpr' e]
    DSLSin e -> renderApplication "sin" [renderDSLExpr' e]
  renderNum :: (Show a, Eq a, Num a) => a -> PDoc
  renderNum n = if signum n == fromInteger (-1)
    then renderPrefix ("-", PrecLevel 6) (renderTerminal . show $ abs n)
    else renderTerminal $ show n
  renderApplication name params = renderPrefixMultiParam (name, PrecLevel 10) params

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
  deriving (Eq, Show)

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

generateContext :: Discretised -> Context
generateContext discretised = Context
  { _contextCellVariables = buildCellVariables discretised mesh solve
  }
  where
  mesh = getSingleton "mesh" (_discretisedMeshes discretised)
  solve = getSingleton "solve" (_meshSolves mesh)

buildCellVariables :: Discretised -> Mesh -> Solve -> [CellVariable]
buildCellVariables discretised mesh solve =
  concatMap (fieldToCellVariables discretised mesh solve) (_meshFields mesh)

getDerivativeName :: String -> Integer -> String
getDerivativeName name d = name ++ "_dt" ++ show d

getPreviousDerivative :: String -> Integer -> DSLExpr
getPreviousDerivative name offset = case offset of
  0 -> DSLCurrent . DSLCellVariable $ getDerivativeName name 0
  _ -> DSLCellVariable$ getDerivativeName name (offset - 1)

generateTimestepping :: Solve -> Update -> String -> Integer -> DSLExpr
generateTimestepping solve update name order = buildDSLExpr translateTerminal expr
  where
  expr = case getTimestepping update order of
    Just e -> e
    Nothing -> error $ "generateTimestepping: missing expression for order " ++ show order
  translateTerminal t = case t of
    PreviousValue -> DSLCellVariable name
    DeltaT -> buildDSLExprDiscreteTerminal $ _solveDeltaT solve
    PreviousDerivative i -> getPreviousDerivative name i

fieldToCellVariables :: Discretised -> Mesh -> Solve -> Field -> [CellVariable]
fieldToCellVariables _discretised _mesh solve field = (cellVariable:cellVariableDerivatives)
  where
  update = findFieldUpdate (FieldLValue (_fieldName field) []) solve
  rhsTensor = _updateRHSDiscrete update
  rhs = Tensor.asScalar rhsTensor
  -- For Euler updating, we do not need to know any previous derivatives, but since we don't incorporate
  -- the derivative directly into the update expression we need to allocate an (unused) derivative.
  maxTemporalOrder = maxTimestepOrder update
  numDerivatives = max (numPreviousTimestepsNeeded update maxTemporalOrder) 1
  name = _fieldName field
  cellVariableDerivatives = [cellVariableDerivative n | n <- [0..numDerivatives-1]]
  cellVariable = CellVariable
    { _cellVariableName = name
    , _cellVariableExpr = generateTimestepping solve update name maxTemporalOrder
    }
  cellVariableDerivative n = CellVariable
    { _cellVariableName = getDerivativeName name n
    , _cellVariableExpr = if n == 0 then buildDSLExprDiscreteTerminal rhs else getPreviousDerivative name n
    }

buildDSLExprDiscreteTerminal :: Expression DiscreteTerminal -> DSLExpr
buildDSLExprDiscreteTerminal = buildDSLExpr expandTerminal
  where
  expandTerminal s = case s of
    FieldDataRef name [] offsets  -> case offsets of
      [0, 0] -> DSLCellVariable name
      [x, y] -> DSLOffset (DSLCellVariable name) (fromIntegral x) (fromIntegral y)
      _ -> error $ "Expected 2D stencil offset expression: " ++ show s
    ConstantDataRef _ _ -> error $ "No non-literal constants expected in FPGA backend (was constant folding applied?): " ++ show s
    FieldDataRef _ _ _  -> error $ "Expected no indices for field index expression (were tensor fields scalarized?): " ++ show s

buildDSLExpr :: Show e => (e -> DSLExpr) -> Expression e -> DSLExpr
buildDSLExpr translateSymbol expr = case expr of
  Symbol s -> translateSymbol s
  Sum seq' -> buildPairSeq (0, (+)) (1, \a b -> a * (buildDSLExpr' $ ConstantRational b)) seq'
  Product seq' -> buildPairSeq (1, (*)) (1, raiseInt) seq'
  ConstantFloat f -> DSLDouble f
  ConstantRational r -> DSLDouble $ fromRational r
  Abs _ -> error $ "unhandled expression: " ++ show expr
  Signum _ -> error $ "unhandled expression: " ++ show expr
  Ln _ -> error $ "unhandled expression: " ++ show expr
  Diff _ _ _ -> error $ "unhandled expression: " ++ show expr
  Int _ _ -> error $ "unhandled expression: " ++ show expr
  Function _ _ -> error $ "unhandled expression: " ++ show expr
  where
  buildDSLExpr' = buildDSLExpr translateSymbol
  raiseInt e p = if denominator p == 1
    then DSLIntPower e (fromIntegral $ numerator p)
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
buildDictionary context =
  Template.insert "fields" (Template.ListVal $ Template.DictVal <$> fieldDictionaries) Template.emptyDict
  where
  fieldDictionaries = buildCellVariableDictionary <$> _contextCellVariables context

buildCellVariableDictionary :: CellVariable -> Template.Dict
buildCellVariableDictionary cellVariable = Map.fromList
  [ ("name", Template.StringVal name)
  , ("update", Template.StringVal . renderDSLExpr $ _cellVariableExpr cellVariable)
  ]
  where
  name = _cellVariableName cellVariable

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
