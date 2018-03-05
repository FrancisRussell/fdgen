module FDGEN.FPGADSLBackend (FPGADSLBackend(..)) where
import FDGEN.Backend(Backend(..))
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import FDGEN.Discrete (Discretised(..), Mesh(..), Field(..), Solve(..), FieldLValue(..), findFieldUpdate, numPreviousTimestepsNeeded)
import Control.Applicative ((<$>))
import qualified FDGEN.Template as Template
import qualified FDGEN.Tensor as Tensor
import qualified Data.Map.Strict as Map

data FPGADSLBackend = FPGADSLBackend

data Context = Context {
  _contextCellVariables :: [CellVariable]
} deriving Show

data CellVariable = CellVariable
  { _cellVariableName :: String
} deriving Show

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

fieldToCellVariables :: Discretised -> Mesh -> Solve -> Field -> [CellVariable]
fieldToCellVariables discretised mesh solve field = (cellVariable:cellVariableDerivatives)
  where
  update = findFieldUpdate (FieldLValue (_fieldName field) []) solve
  --_rhsTensor = findFieldUpdateRHS (FieldLValue (_fieldName field) []) solve
  --_rhs = Tensor.asScalar rhsTensor
  numDerivatives = numPreviousTimestepsNeeded update
  cellVariableDerivatives = [cellVariableDerivative n | n <- [0..numDerivatives-1]]
  cellVariable = CellVariable
    { _cellVariableName = _fieldName field
    }
  cellVariableDerivative n = CellVariable
    { _cellVariableName = getDerivativeName (_fieldName field) n
    }

buildDictionary :: Context -> Template.Dict
buildDictionary context =
  Template.insert "fields" (Template.ListVal $ Template.DictVal <$> fieldDictionaries) Template.emptyDict
  where
  fieldDictionaries = buildCellVariableDictionary <$> _contextCellVariables context

buildCellVariableDictionary :: CellVariable -> Template.Dict
buildCellVariableDictionary cellVariable = Map.fromList [ ("name", Template.StringVal name) ]
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
    context = generateContext discreteForm
