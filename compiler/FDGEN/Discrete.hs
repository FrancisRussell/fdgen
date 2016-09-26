module FDGEN.Discrete (buildDiscreteForm) where
import FDGEN.Algebra (Expression)
import FDGEN.Tensor (Tensor, TensorIndex)
import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map
import qualified FDGEN.Parser as Parser

data Terminal
  = FieldRef String TensorIndex
  deriving Show

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

data Update = Update {
  _updateLHS :: Expression Terminal,
  _updateRHS :: Expression Terminal
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
  , _meshFields = (buildField fdfl . getFieldDef) <$> (Parser._meshFields mesh)
  , _meshSolves = [] -- TODO: implement me
  }
  where
  getFieldDef ident = case Parser.getSymbol fdfl $ Parser.identifierValue ident of
    Just (Parser.FieldDef field) -> field
    _ -> error $ "buildMesh: unknown field " ++ Parser.identifierValue ident

buildField :: Parser.FDFL -> Parser.Field -> Field
buildField fdfl field = Field
  { _fieldName = Parser.stringLiteralValue $ Parser._fieldName field
  , _fieldRank = Parser._fieldRank field
  , _fieldSymmetric = Parser._fieldSymmetric field
  , _fieldStaggerSpatial = [] -- TODO: implement me
  , _fieldStaggerTemporal = False -- TODO: implement me
  }
