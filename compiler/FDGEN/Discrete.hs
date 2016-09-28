module FDGEN.Discrete (buildDiscreteForm) where
import FDGEN.Algebra (Expression(..), diff)
import FDGEN.Tensor (Tensor, TensorIndex)
import FDGEN.Pretty (PrettyPrintable(..), structureDoc, hListDoc, vListDoc)
import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)
import Data.List (genericIndex)
import qualified Data.Map.Strict as Map
import qualified FDGEN.Parser as Parser
import qualified FDGEN.Tensor as Tensor
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

data Mesh = Mesh {
  _meshName :: String,
  _meshDimension :: Integer,
  _meshFields :: [Field],
  _meshSolves :: [Solve]
} deriving Show

instance PrettyPrintable Mesh
  where
  toDoc mesh = structureDoc "Mesh"
    [ ("name", toDoc $ _meshName mesh)
    , ("dim", toDoc $ _meshDimension mesh)
    , ("fields", vListDoc $ _meshFields mesh)
    , ("solves", vListDoc $ _meshSolves mesh)
    ]

data Field = Field {
  _fieldName :: String,
  _fieldRank :: Integer,
  _fieldSymmetric :: Bool,
  _fieldStaggerSpatial :: [Bool],
  _fieldStaggerTemporal :: Bool
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

data Solve = Solve {
  _solveName :: String,
  _solveSpatialOrder :: Integer,
  _solveTemporalOrder :: Integer,
  _solveUpdates :: [Update]
} deriving Show

instance PrettyPrintable Solve
  where
  toDoc solve = structureDoc "Solve"
    [ ("name", toDoc $ _solveName solve)
    , ("spatial_order", toDoc $ _solveSpatialOrder solve)
    , ("temporal_order", toDoc $ _solveTemporalOrder solve)
    , ("updates", vListDoc $ _solveUpdates solve)
    ]

data FieldTemporalDerivative
  = FieldTemporalDerivative String Integer
  deriving (Eq, Ord, Show)

instance PrettyPrintable FieldTemporalDerivative
  where
  toDoc (FieldTemporalDerivative f i) = PrettyPrint.hcat $ PrettyPrint.text <$> ["(d^", i', " * ", f, ")/dt^", i']
    where
    i' = show i

data Update = Update
  { _updateLHS :: FieldTemporalDerivative
  , _updateRHS :: Tensor (Expression Terminal)
  } deriving Show

instance PrettyPrintable Update
  where
  toDoc update = structureDoc "Update"
    [ ("lhs", toDoc $ _updateLHS update)
    , ("rhs", toDoc $ _updateRHS update)
    ]

buildDiscreteForm :: Parser.FDFL -> Discretised
buildDiscreteForm fdfl = Discretised { _discretisedMeshes = catMaybes maybeMeshes }
  where
  maybeMeshes = maybeBuildMesh <$> Map.elems (Parser.getSymbols fdfl)
  maybeBuildMesh (Parser.MeshDef meshDef) = Just $ buildMesh fdfl meshDef
  maybeBuildMesh _ = Nothing

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
 , _solveTemporalOrder = Parser._solveTemporalOrder solve
 , _solveUpdates = (buildUpdate fdfl mesh . getExpressionDef) <$> (Parser._solveEquations solve)
 }
 where
 getExpressionDef ident = case Parser.getSymbol fdfl ident of
   Just (Parser.EquationDef equ) -> equ
   _ -> error $ "buildSolve: unknown equation " ++ Parser.identifierValue ident

buildUpdate :: Parser.FDFL -> Parser.Mesh -> Parser.Equation -> Update
buildUpdate fdfl mesh equ = Update
  { _updateLHS = buildLHS $ Parser._fieldUpdateLHS equ
  , _updateRHS = buildRHS $ Parser._fieldUpdateRHS equ
  }
  where
  getFieldName = Parser.stringLiteralValue . Parser._fieldName
  dimension = Parser._meshDimension mesh
  buildLHS expr = case expr of
    (Parser.FieldTemporalDerivative (Parser.FieldRef ident)) ->
      FieldTemporalDerivative (getFieldName $ Parser.getFieldDef fdfl ident) 1
    _ -> error $ "Unsupported LHS: " ++ show expr
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
