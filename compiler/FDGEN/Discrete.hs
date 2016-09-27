module FDGEN.Discrete (buildDiscreteForm) where
import FDGEN.Algebra (Expression)
import FDGEN.Tensor (Tensor, TensorIndex)
import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map
import qualified FDGEN.Parser as Parser

data Terminal
  = FieldRef String TensorIndex
  | ConstantRef String
  deriving (Eq, Ord, Show)

data FieldAccess = FieldAccess {
  _fieldAccessName :: String,
  _fieldAccessTemporalIndex :: Integer,
  _fieldAccessSpatialOffsets :: [Integer]
} deriving Show

data Mesh = Mesh {
  _meshName :: String,
  _meshDimension :: Integer,
  _meshFields :: [Field],
  _meshSolves :: [Solve]
} deriving Show

data Field = Field {
  _fieldName :: String,
  _fieldRank :: Integer,
  _fieldSymmetric :: Bool,
  _fieldStaggerSpatial :: [Bool],
  _fieldStaggerTemporal :: Bool
} deriving Show

data Solve = Solve {
  _solveName :: String,
  _solveSpatialOrder :: Integer,
  _solveTemporalOrder :: Integer,
  _solveUpdates :: [Update]
} deriving Show

data FieldTemporalDerivative
  = FieldTemporalDerivative String Integer
  deriving (Eq, Ord, Show)

data Update = Update
  { _updateLHS :: FieldTemporalDerivative
  , _updateRHS :: Expression Terminal
  } deriving Show

buildDiscreteForm :: Parser.FDFL -> [Mesh]
buildDiscreteForm fdfl = catMaybes maybeMeshes
  where
  maybeMeshes = maybeBuildMesh <$> Map.elems (Parser.getSymbols fdfl)
  maybeBuildMesh (Parser.MeshDef meshDef) = Just $ buildMesh fdfl meshDef
  maybeBuildMesh _ = Nothing

buildMesh :: Parser.FDFL -> Parser.Mesh -> Mesh
buildMesh fdfl mesh = Mesh
  { _meshName = Parser.stringLiteralValue $ Parser._meshName mesh
  , _meshDimension = Parser._meshDimension mesh
  , _meshFields = (buildField fdfl . Parser.getFieldDef fdfl) <$> (Parser._meshFields mesh)
  , _meshSolves = (buildSolve fdfl . getSolveDef) <$> (Parser._meshSolves mesh)
  }
  where
  getSolveDef ident = case Parser.getSymbol fdfl ident of
    Just (Parser.SolveDef solve) -> solve
    _ -> error $ "buildMesh: unknown solve " ++ Parser.identifierValue ident

buildField :: Parser.FDFL -> Parser.Field -> Field
buildField fdfl field = Field
  { _fieldName = Parser.stringLiteralValue $ Parser._fieldName field
  , _fieldRank = Parser._fieldRank field
  , _fieldSymmetric = Parser._fieldSymmetric field
  , _fieldStaggerSpatial = [] -- TODO: implement me
  , _fieldStaggerTemporal = False -- TODO: implement me
  }

buildSolve :: Parser.FDFL -> Parser.Solve -> Solve
buildSolve fdfl solve = Solve
 { _solveName = Parser.stringLiteralValue $ Parser._solveName solve
 , _solveSpatialOrder = Parser._solveSpatialOrder solve
 , _solveTemporalOrder = Parser._solveTemporalOrder solve
 , _solveUpdates = (buildUpdate fdfl . getExpressionDef) <$> (Parser._solveEquations solve)
 }
 where
 getExpressionDef ident = case Parser.getSymbol fdfl ident of
   Just (Parser.EquationDef equ) -> equ
   _ -> error $ "buildSolve: unknown equation " ++ Parser.identifierValue ident

buildUpdate :: Parser.FDFL -> Parser.Equation -> Update
buildUpdate fdfl equ = Update
  { _updateLHS = buildLHS $ Parser._fieldUpdateLHS equ
  , _updateRHS = buildRHS $ Parser._fieldUpdateRHS equ
  }
  where
  getFieldName = Parser.stringLiteralValue . Parser._fieldName
  buildLHS expr = case expr of
    (Parser.FieldTemporalDerivative (Parser.FieldRef ident)) ->
      FieldTemporalDerivative (getFieldName $ Parser.getFieldDef fdfl ident) 1
    _ -> error $ "Unsupported LHS: " ++ show expr
  buildRHS expr = case expr of
    _ -> fromInteger 0
