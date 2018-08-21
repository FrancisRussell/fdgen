{-# LANGUAGE TemplateHaskell, Rank2Types, FlexibleInstances, FlexibleContexts #-}
module FDGEN.Parser ( parseInput, FDFL, getSymbols, Definition(..)
                    , Mesh(..), stringLiteralValue, identifierValue
                    , getSymbol, Field(..), Solve(..), Equation(..)
                    , FieldExpr(..), getFieldDef, LiteralConstant(..)
                    , MeshConstant(..), Identifier(..), StaggerStrategy(..)
                    , BoundaryCondition(..), NamedLiteral(..)
                    , StringLiteral) where
import Data.Char (toLower, digitToInt)
import Data.Map (Map)
import Data.Maybe (isJust)
import Data.List (genericIndex)
import Control.Applicative ((<$>))
import Control.Monad ((>=>))
import Text.Parsec.Char (letter, spaces, char, digit)
import Text.Parsec.Combinator (eof, choice, optionMaybe, many1, option)
import Text.Parsec.Language (emptyDef, LanguageDef)
import Text.Parsec (ParsecT, ParseError, runParser, getState, Parsec, putState, try, Stream)
import Text.Parsec.Prim (many, parserFail)
import Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))
import Text.Parsec.Token (GenTokenParser(..), GenLanguageDef(..), makeTokenParser)
import FDGEN.Pretty (PrettyPrintable(..), structureDoc, hListDoc, hPairDoc)
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.PrettyPrint as PrettyPrint

-- |An identifier in an FDFL file
data Identifier = Identifier String
  deriving (Show, Ord, Eq)

instance PrettyPrintable Identifier where
  toDoc (Identifier i) = PrettyPrint.text i

-- |A string literal in an FDFL file
data StringLiteral = StringLiteral String
  deriving (Show, Ord, Eq)

instance PrettyPrintable StringLiteral where
  toDoc (StringLiteral s) = PrettyPrint.text $ show s

-- |Converts a string literal to a string
stringLiteralValue :: StringLiteral -> String
stringLiteralValue (StringLiteral v) = v

-- |Converts an identifier to a string
identifierValue :: Identifier -> String
identifierValue (Identifier v) = v

-- |Parser-level representation of a mesh
data Mesh = Mesh
  { _meshName :: StringLiteral -- ^ The name of the mesh
  , _meshDimension :: Integer -- ^ The dimension of the mesh
  , _meshFields :: [Identifier] -- ^ A list of fields present on the mesh
  , _meshSolves :: [Identifier] -- ^ A list of solve operations that can be applied to the mesh
  , _meshGridSpacing :: [FieldExpr Identifier] -- ^ Expressions describing the distance between grid points in each dimension
  , _meshGridDimensions :: [FieldExpr Identifier] -- ^ Expressions describing the number of cells (not points) in each dimension
  , _meshInitialValues :: [(StringLiteral, FieldExpr Identifier)] -- ^ A list of name-value pairs giving an initial value for specified fields (otherwise all components are 0-valued)
} deriving Show

-- |Parser-level representation of complete simulation
data Simulation = Simulation
  { _simMesh :: Identifier -- ^ The mesh on which the simulation occurs
  , _simWantedFields :: [Identifier] -- ^ The fields whose values should be sampled
  , _simTimesteps :: Integer -- ^ The number of time steps to execute the simulation for
  , _simSampleGap :: Integer -- ^ The number of time steps between each sample
  , _simWarmUp :: Integer -- ^ The number of time steps to discard from the start of the simulation
  } deriving Show

instance PrettyPrintable Mesh
  where
  toDoc mesh = structureDoc "Mesh"
    [ ("name", toDoc $ _meshName mesh)
    , ("dim", toDoc $ _meshDimension mesh)
    , ("fields", hListDoc $ _meshFields mesh)
    , ("solves", hListDoc $ _meshSolves mesh)
    , ("spacing", hListDoc $ _meshGridSpacing mesh)
    , ("dimensions", hListDoc $ _meshGridDimensions mesh)
    , ("initial", hListDoc $ hPairDoc <$> _meshInitialValues mesh)
    ]

instance PrettyPrintable Simulation
  where
  toDoc sim = structureDoc "Simulation"
    [ ("mesh", toDoc $ _simMesh sim)
    , ("wanted_fields", hListDoc $ _simWantedFields sim)
    , ("timesteps", toDoc $ _simTimesteps sim)
    , ("sample_gap", toDoc $ _simSampleGap sim)
    , ("warm_up", toDoc $ _simWarmUp sim)
    ]

-- |Parser-level representation of a field
data Field = Field
  { _fieldName :: StringLiteral -- ^ The name of the field
  , _fieldRank :: Integer -- ^ The tensor rank of a field
  , _fieldSymmetric :: Bool -- ^ Whether the field is symmetric (ignored)
  , _fieldInitial :: Maybe (FieldExpr Identifier) -- ^ An initial expression for a field (overriden by any specified by a mesh)
  , _fieldStaggerStrategySpatial :: StaggerStrategy -- ^ The spatial staggering strategy for the scalar components
  , _fieldStaggerStrategyTemporal :: StaggerStrategy -- ^ The temporal staggering strategy for the scalar components (ignored)
  } deriving Show

instance PrettyPrintable Field
  where
  toDoc field = structureDoc "Field" $
    [ ("name", toDoc $ _fieldName field)
    , ("rank", toDoc $ _fieldRank field)
    , ("symmetric", toDoc $ _fieldSymmetric field)
    , ("spatial_stagger", toDoc $ _fieldStaggerStrategySpatial field)
    , ("temporal_stagger", toDoc $ _fieldStaggerStrategyTemporal field)
    ] ++ maybeInitial
    where
    maybeInitial = case _fieldInitial field of
      Nothing -> []
      Just expr -> [("initial", toDoc expr)]

-- | A general expression type that can represent tensor-valued field expressions, but also scalar valued and
-- mesh-independent expressions.
data FieldExpr a
  = FieldRef a -- ^ A reference to a field
  | FieldLiteral LiteralConstant -- ^ A constant tensor-valued quantity
  | FieldAddition (FieldExpr a) (FieldExpr a) -- ^ Addition of identical rank fields
  | FieldDivision (FieldExpr a) (FieldExpr a) -- ^ Division of field by a scalar
  | FieldInner (FieldExpr a) (FieldExpr a) -- ^ Inner product
  | FieldOuter (FieldExpr a) (FieldExpr a) -- ^ Outer product
  | FieldDot (FieldExpr a) (FieldExpr a) -- ^ Dot product
  | FieldGradient (FieldExpr a) -- ^ Gradient operator
  | FieldDivergence (FieldExpr a) -- ^ Divergence operator
  | FieldSpatialDerivative (FieldExpr a) Integer -- ^ Spatial derivative in specified dimension
  | FieldTemporalDerivative (FieldExpr a) -- ^ Temporal derivative
  | FieldNormalDerivative (FieldExpr a) -- ^ Normal derivative (only valid at an edge)
  | FieldIndexOperation [Integer] (FieldExpr a) -- ^ Tensor indexing operation
  | FieldTensorElements [FieldExpr a] -- ^ Constructs a field from a list of fields of lower rank
  | FieldPower (FieldExpr a) (FieldExpr a) -- ^ Scalar power operation
  | FieldExponent (FieldExpr a) -- ^ Raises e to the scalar operand
  | FieldCos (FieldExpr a) -- ^ Cosine on scalar operand
  | FieldSin (FieldExpr a) -- ^ Sine on scalar operand
  | FieldTan (FieldExpr a) -- ^ Tangent on scalar operand
  | FieldPi -- ^ The constant Pi
  | FieldPosition -- ^ Position coordinate on the mesh
  deriving Show

instance PrettyPrintable a => PrettyPrintable (FieldExpr a)
  where
  toDoc expr = case expr of
    FieldRef a -> toDoc a
    FieldAddition a b -> binaryOp "+" a b
    FieldDivision a b -> binaryOp "/" a b
    FieldInner a b -> function "inner" [a, b]
    FieldOuter a b -> function "outer" [a, b]
    FieldDot a b -> function "dot" [a, b]
    FieldGradient a -> function "grad" [a]
    FieldDivergence a -> function "div" [a]
    FieldSpatialDerivative a i -> function "diff" [toDoc a, dim i]
    FieldTemporalDerivative a -> function "dt" [a]
    FieldNormalDerivative a -> function "dn" [a]
    FieldLiteral (ScalarConstant r) -> PrettyPrint.double $ fromRational r
    FieldLiteral PermutationSymbol -> text "epsilon"
    FieldIndexOperation indices a -> hcat [toDoc a , hListDoc indices]
    FieldTensorElements elements -> hcat [text "[", printList elements, text "]"]
    FieldPower a b -> function "pow" [toDoc a, toDoc b]
    FieldExponent a -> function "exp" [toDoc a]
    FieldCos a -> function "cos" [toDoc a]
    FieldSin a -> function "sin" [toDoc a]
    FieldTan a -> function "tan" [toDoc a]
    FieldPi -> text "pi"
    FieldPosition -> text "pos"
    where
      text = PrettyPrint.text
      hcat = PrettyPrint.hcat
      dim x = genericIndex (text <$> ["x", "y", "z"] ++ ["dim(" ++ show n ++ ")" | n <- [3..] :: [Integer]]) x
      binaryOp op a b = hcat [text "(", toDoc a, text $ " " ++ op ++ " ", toDoc b, text ")"]
      printList elements = hcat $ PrettyPrint.punctuate (text ", ") (toDoc <$> elements)
      function name params = hcat [prefix, printList params, suffix]
        where
        prefix = text $ name ++ "("
        suffix = text ")"

-- | A tensor valued compile-time constant value
data LiteralConstant
  = ScalarConstant Rational -- ^ A scalar-valued rational value
  | PermutationSymbol -- ^ The permutation symbol of appropriate rank
  deriving Show

-- | A compile-time constant value given a name to ease debugging
data NamedLiteral = NamedLiteral
  { _namedLiteralName :: StringLiteral -- ^ The name of the value e.g. "g"
  , _namedLiteralValue :: Rational -- ^ The literal value e.g. "9.81"
  } deriving Show

instance PrettyPrintable NamedLiteral
 where
 toDoc literal = structureDoc "NamedLiteral"
   [ ("name", toDoc $ _namedLiteralName literal)
   , ("value", toDoc  ((fromRational $ _namedLiteralValue literal)::Double))
   ]

-- | A tensor-valued quantity that is constant over the mesh, but not necessarily compile-time constant. The backend
-- will need to provide a mechanism so that such values can be set.
data MeshConstant = MeshConstant {
  _meshConstantRank :: Integer, -- ^ The constant's rank
  _meshConstantName :: StringLiteral -- ^ The constant's name
} deriving Show

instance PrettyPrintable MeshConstant
 where
 toDoc constant = structureDoc "MeshConstant"
   [ ("rank", toDoc $ _meshConstantRank constant)
   , ("name", toDoc $ _meshConstantName constant)
   ]

-- | Represents a equation defining either the value of a field
data Equation = Equation {
  _fieldUpdateLHS :: FieldExpr Identifier, -- ^ The value being defined
  _fieldUpdateRHS :: FieldExpr Identifier -- ^ The value to be assigned
} deriving Show

instance PrettyPrintable Equation
 where
 toDoc equ = structureDoc "Equation"
   [ ("lhs", toDoc $ _fieldUpdateLHS equ)
   , ("rhs", toDoc $ _fieldUpdateRHS equ)
   ]

-- | Represents a boundary condition
data BoundaryCondition = BoundaryCondition {
  _bcLHS :: FieldExpr Identifier, -- ^ The boundary being defined
  _bcRHS :: FieldExpr Identifier, -- ^ The value for the boundary
  _bcSubdomains :: Maybe [StringLiteral] -- ^ The names of the boundary subdomains
} deriving Show

instance PrettyPrintable BoundaryCondition
 where
 toDoc bc = structureDoc "BoundaryCondition"
   [ ("lhs", toDoc $ _bcLHS bc)
   , ("rhs", toDoc $ _bcRHS bc)
   ]

-- | Represents the mesh update operation applied in a single time step.
data Solve = Solve
  { _solveName :: StringLiteral -- ^ The name of the solve
  , _solveSpatialOrder :: Integer -- ^ The order of spatial accuracy to be used
  , _solveTemporalOrder :: Integer -- ^ The order of temporal accuracy to be used
  , _solveEquations :: [Identifier] -- ^ List of equations used to update fields
  , _solveBoundaryConditions :: [Identifier] -- ^ List of boundary conditions to be applied
  , _solveDeltaT :: FieldExpr Identifier -- ^ Expression for size of the timestep
} deriving Show

instance PrettyPrintable Solve
 where
 toDoc solve = structureDoc "Solve"
   [ ("name", toDoc $ _solveName solve)
   , ("spatial_order", toDoc $ _solveSpatialOrder solve)
   , ("temporal_order", toDoc $ _solveTemporalOrder solve)
   , ("equations", hListDoc $ _solveEquations solve)
   , ("boundary_conditions", hListDoc $ _solveBoundaryConditions solve)
   , ("delta_t", toDoc $ _solveDeltaT solve)
   ]

-- | Each FDFL identifier is associated with a definition
data Definition
 = FieldDef Field
 | MeshDef Mesh
 | EquationDef Equation
 | BoundaryConditionDef BoundaryCondition
 | MeshConstantDef MeshConstant
 | NamedLiteralDef NamedLiteral
 | FieldExprDef (FieldExpr Identifier)
 | SolveDef Solve
 | SimulationDef Simulation
 deriving Show

instance PrettyPrintable Definition
 where
 toDoc def = case def of
   FieldDef f -> toDoc f
   MeshDef m -> toDoc m
   EquationDef e -> toDoc e
   BoundaryConditionDef bc -> toDoc bc
   MeshConstantDef c -> toDoc c
   NamedLiteralDef l -> toDoc l
   FieldExprDef f -> toDoc  f
   SolveDef s -> toDoc s
   SimulationDef s -> toDoc s

-- | A strategy for staggering scalar components for a tensor
data StaggerStrategy
  = All -- ^ All components are staggered
  | None -- ^ None of the components are staggered
  | Dimension -- ^ Each component is staggered in its respective dimension (only valid for rank-1 tensors)
  deriving (Bounded, Enum, Show)

instance PrettyPrintable StaggerStrategy where
  toDoc = PrettyPrint.text . show

-- | Parser representation of a parsed FDFL file
data FDFL = FDFL {
  symbols :: Map String Definition -- ^ Maps identifiers to definitions
} deriving Show

instance PrettyPrintable FDFL
  where
  toDoc fdfl = structureDoc "FDFL" fields
    where
    fields = fieldDoc <$> (Map.assocs $ symbols fdfl)
    fieldDoc (name, value) = (name, toDoc value)

-- | State used by the FDFL parser (currently just an FDFL that is populated)
data FDFLParseState = FDFLParseState {
  _psFDFL :: FDFL
} deriving Show

Lens.makeLenses ''FDFLParseState
Lens.makeLenses ''Mesh
Lens.makeLenses ''Field
Lens.makeLenses ''Equation
Lens.makeLenses ''MeshConstant
Lens.makeLenses ''Solve
Lens.makeLenses ''BoundaryCondition
Lens.makeLenses ''NamedLiteral
Lens.makeLenses ''Simulation

-- | Builds a valid unpopulated FDFL
emptyFDFL :: FDFL
emptyFDFL = FDFL {
  symbols = Map.empty
}

-- | Returns the identifier to definition mapping of an FDFL
getSymbols :: FDFL -> Map String Definition
getSymbols = symbols


-- | Constructs the initial state for the FDFL parser
emptyFDFLParseState :: FDFLParseState
emptyFDFLParseState = FDFLParseState {
  _psFDFL = emptyFDFL
}

-- | The type of an FDFL sub-parser that returns an a
type FDFLParser a = Parsec String FDFLParseState a

-- | Type of an action used to validate a parsed value and throw an error at the parser-level if it is invalid
type Validator a = a -> FDFLParser a

-- | The result of parsing a positional parameter or key-value mapping in an FDFL file is an 'AttributeUpdate'. This contains a function
-- that knows how to update a field of some 'FDFLObject' of type 's'. Since this function is opaque, we also store an attribute name, which
-- can be used to determine if multiple updates for the same attribute exist.
data AttributeUpdate s = AttributeUpdate String (s -> s)

-- | Defines a positional parameter of the given name and a parser for it.
data PositionalSpec s = PositionalSpec String (FDFLParser (AttributeUpdate s))

-- | Defines an attribute parameter and a parser for it. The boolean parameter
-- specifies if this attribute is required.
data AttributeSpec s = AttributeSpec String Bool (FDFLParser (AttributeUpdate s))

data ObjectParseSpec s
  = ObjectParseSpec String [PositionalSpec s] [AttributeSpec s]

buildAttributeSpec :: FDFLParsable a => String -> Bool -> Validator a
  -> Lens.Setter' s a -> AttributeSpec s
buildAttributeSpec name required validator setter =
  AttributeSpec name required (parseKeywordParam name validator setter)

buildPositionalSpec :: FDFLParsable a => String -> Validator a
  -> Lens.Setter' s a -> PositionalSpec s
buildPositionalSpec name validator setter =
  PositionalSpec name (parsePositionalParam name validator setter)

alwaysValid :: Validator a
alwaysValid = return

validateList :: Validator a -> Validator [a]
validateList = mapM

findDuplicates :: Ord a => [a] -> [a]
findDuplicates keys = Map.keys $ Map.filter (> 1) histogram
  where
  insertWith' m (k, a) = Map.insertWith (+) k a m
  histogram = foldl insertWith' Map.empty [(k, 1::Integer) | k <- keys]

noDuplicates :: (Show a, Ord a) => Validator [a]
noDuplicates entries = do
  case findDuplicates entries of
   [] -> return entries
   (firstDuplicate:_) -> parserFail $ "Unexpected duplicate entry: " ++ show firstDuplicate

validateDefinition :: (Definition -> Bool) -> String -> Validator Identifier
validateDefinition validate friendlyType name = do
  state <- getState
  let fdfl = _psFDFL state
  case getSymbol fdfl name of
    Nothing -> parserFail $ "Unknown identifier " ++ identifierValue name
    Just def -> if validate def
      then return $ name
      else parserFail $ identifierValue name ++ " should be of type " ++ friendlyType ++ " but is not."

isFieldLike :: Validator Identifier
isFieldLike = validateDefinition validate "field"
  where validate def = case def of
                         FieldDef _ -> True
                         FieldExprDef _ -> True
                         MeshConstantDef _ -> True
                         NamedLiteralDef _ -> True
                         _ -> False

isField :: Validator Identifier
isField = validateDefinition validate "field"
  where validate def = case def of
                         FieldDef _ -> True
                         _ -> False


isSolve :: Validator Identifier
isSolve = validateDefinition validate "solve"
  where validate def = case def of
                         SolveDef _ -> True
                         _ -> False

isEquation :: Validator Identifier
isEquation = validateDefinition validate "equation"
  where validate def = case def of
                         EquationDef _ -> True
                         _ -> False

isBoundaryCondition :: Validator Identifier
isBoundaryCondition = validateDefinition validate "boundary condition"
  where validate def = case def of
                         BoundaryConditionDef _ -> True
                         _ -> False

isMesh :: Validator Identifier
isMesh = validateDefinition validate "mesh"
  where validate def = case def of
                         MeshDef _ -> True
                         _ -> False

parseKeywordParam :: FDFLParsable a => String -> Validator a
  -> Lens.Setter' s a -> FDFLParser (AttributeUpdate s)
parseKeywordParam name validator setter =  do
  _ <- parseReserved name >> parseReservedOp "="
  AttributeUpdate name <$> Lens.set' setter <$> (parse >>= validator)

parsePositionalParam :: FDFLParsable a => String -> Validator a
  -> Lens.Setter' s a -> FDFLParser (AttributeUpdate s)
parsePositionalParam name validator setter =
  AttributeUpdate name <$> Lens.set' setter <$> (parse >>= validator)

parseField :: ObjectParseSpec Field
parseField = ObjectParseSpec "Field" []
  [ buildAttributeSpec "rank" True alwaysValid fieldRank
  , buildAttributeSpec "name" True alwaysValid fieldName
  , buildAttributeSpec "symmetric" False alwaysValid fieldSymmetric
  , buildAttributeSpec "spatial_staggering" False alwaysValid fieldStaggerStrategySpatial
  , buildAttributeSpec "temporal_staggering" False alwaysValid fieldStaggerStrategyTemporal
  , buildAttributeSpec "initial" False alwaysValid fieldInitial
  ]

parseMesh :: ObjectParseSpec Mesh
parseMesh = ObjectParseSpec "Mesh" []
  [ buildAttributeSpec "name" True alwaysValid meshName
  , buildAttributeSpec "dim" True alwaysValid meshDimension
  , buildAttributeSpec "fields" True (validateList isFieldLike >=> noDuplicates) meshFields
  , buildAttributeSpec "solves" True (validateList isSolve >=> noDuplicates) meshSolves
  , buildAttributeSpec "spacing" True alwaysValid meshGridSpacing
  , buildAttributeSpec "dimensions" True alwaysValid meshGridDimensions
  , buildAttributeSpec "initial" False alwaysValid meshInitialValues
  ]

parseEquation :: ObjectParseSpec Equation
parseEquation = ObjectParseSpec "Equation"
  [ buildPositionalSpec "lhs" alwaysValid fieldUpdateLHS
  , buildPositionalSpec "rhs" alwaysValid fieldUpdateRHS
  ]
  []

parseBoundaryCondition :: ObjectParseSpec BoundaryCondition
parseBoundaryCondition = ObjectParseSpec "BoundaryCondition"
  [ buildPositionalSpec "lhs" alwaysValid bcLHS
  , buildPositionalSpec "rhs" alwaysValid bcRHS
  ]
  [ buildAttributeSpec "subdomains" False alwaysValid bcSubdomains
  ]

parseMeshConstant :: ObjectParseSpec MeshConstant
parseMeshConstant = ObjectParseSpec "MeshConstant"
  []
  [ buildAttributeSpec "name" True alwaysValid meshConstantName
  , buildAttributeSpec "rank" False alwaysValid meshConstantRank
  ]

parseNamedLiteral :: ObjectParseSpec NamedLiteral
parseNamedLiteral = ObjectParseSpec "NamedLiteral"
  []
  [ buildAttributeSpec "name" True alwaysValid namedLiteralName
  , buildAttributeSpec "value" True alwaysValid namedLiteralValue
  ]

parseSolve :: ObjectParseSpec Solve
parseSolve = ObjectParseSpec "Solve"
  []
  [ buildAttributeSpec "name" True alwaysValid solveName
  , buildAttributeSpec "spatial_order" False alwaysValid solveSpatialOrder
  , buildAttributeSpec "temporal_order" False alwaysValid solveTemporalOrder
  , buildAttributeSpec "equations" False (validateList isEquation >=> noDuplicates) solveEquations
  , buildAttributeSpec "boundary_conditions" False (validateList isBoundaryCondition >=> noDuplicates) solveBoundaryConditions
  , buildAttributeSpec "delta_t" True alwaysValid solveDeltaT
  ]

parseSimulation :: ObjectParseSpec Simulation
parseSimulation = ObjectParseSpec "Simulation"
  []
  [ buildAttributeSpec "mesh" True isMesh simMesh
  -- FIXME: no way to validate if mesh contains specified fields
  , buildAttributeSpec "wanted_fields" True  (validateList isField) simWantedFields
  , buildAttributeSpec "num_timesteps" True alwaysValid simTimesteps
  , buildAttributeSpec "sample_every" False alwaysValid simSampleGap
  , buildAttributeSpec "warm_up" False alwaysValid simWarmUp
  ]

validateAttributes :: [AttributeSpec s] -> [AttributeUpdate s]
  -> Either String [AttributeUpdate s]
validateAttributes specs updates = if not $ null duplicateAttributes
  then Left $ "Duplicate attribute " ++ head duplicateAttributes
  else if not $ null missing
    then Left $ "Missing attribute " ++ head missing
    else Right updates
  where
  reqAttributes = [name | AttributeSpec name req _ <- specs, req]
  present = Set.fromList attributeNames
  missing = [name | name <- reqAttributes, not $ Set.member name present]
  attributeNames = [name | AttributeUpdate name _ <- updates]
  duplicateAttributes = findDuplicates attributeNames

parseCommaSepSeq :: [FDFLParser a] -> FDFLParser [a]
parseCommaSepSeq [] = return []
parseCommaSepSeq [p] = return <$> p
parseCommaSepSeq (p:ps) = do
  first <- p
  _ <- parseComma
  rest <- parseCommaSepSeq ps
  return (first:rest)

parseSpecToParser :: FDFLObject s => ObjectParseSpec s -> FDFLParser Definition
parseSpecToParser (ObjectParseSpec name positionalSpecs attributeSpecs) =
  parseReserved name >> parseParens parseParams
  where parseParams = do
        let positionalParsers = [parser | (PositionalSpec _ parser) <- positionalSpecs]
        let keywordParsers = [parser | (AttributeSpec _ _ parser) <- attributeSpecs]
        positionalAttributes <- parseCommaSepSeq positionalParsers
        maybeComma <- optionMaybe parseComma
        let parseKeywords = case maybeComma of
                              Just _ -> True
                              Nothing -> null positionalAttributes
        let unvalidatedAttributes = parseCommaSep $ choice keywordParsers
        attributes <- if parseKeywords
          then parserFailEither $ validateAttributes attributeSpecs <$> unvalidatedAttributes
          else return []
        let allAttributes = attributes ++ positionalAttributes
        let updates = [update | (AttributeUpdate _ update) <- allAttributes]
        return . wrapObject $ foldl (flip ($)) emptyObject updates

class FDFLObject a where
  wrapObject :: a -> Definition
  emptyObject :: a

instance FDFLObject Field where
  wrapObject = FieldDef
  emptyObject = Field
    { _fieldSymmetric = False
    , _fieldName = error "undefined fieldName"
    , _fieldRank = error "undefined fieldRank"
    , _fieldInitial = Nothing
    , _fieldStaggerStrategySpatial = None
    , _fieldStaggerStrategyTemporal = None
    }

instance FDFLObject Mesh where
  wrapObject = MeshDef
  emptyObject = Mesh
    { _meshName = error "undefined meshName"
    , _meshDimension = error "undefined meshDimension"
    , _meshFields = []
    , _meshSolves = []
    , _meshGridSpacing = error "undefined meshGridSpacing"
    , _meshGridDimensions = error "undefined meshGridDimensions"
    , _meshInitialValues = []
    }

instance FDFLObject Equation where
  wrapObject = EquationDef
  emptyObject = Equation
    { _fieldUpdateLHS = error "undefined fieldUpdateLHS"
    , _fieldUpdateRHS = error "undefined fieldUpdateRHS"
    }

instance FDFLObject BoundaryCondition where
  wrapObject = BoundaryConditionDef
  emptyObject = BoundaryCondition
    { _bcLHS = error "undefined bcLHS"
    , _bcRHS = error "undefined bcRHS"
    , _bcSubdomains = Nothing
    }

instance FDFLObject MeshConstant where
  wrapObject = MeshConstantDef
  emptyObject = MeshConstant
    { _meshConstantName = error "undefined constantName"
    , _meshConstantRank = 0
    }

instance FDFLObject NamedLiteral where
  wrapObject = NamedLiteralDef
  emptyObject = NamedLiteral
    { _namedLiteralName = error "undefined namedLiteralName"
    , _namedLiteralValue = error "undefined namedLiteralValue"
    }

instance FDFLObject Solve where
  wrapObject = SolveDef
  emptyObject = Solve
    { _solveName = error "undefined solveName"
    , _solveSpatialOrder = 1
    , _solveTemporalOrder = 1
    , _solveEquations = []
    , _solveBoundaryConditions = []
    , _solveDeltaT = error "undefined solveDeltaT"
    }

instance FDFLObject Simulation where
  wrapObject = SimulationDef
  emptyObject = Simulation
    { _simMesh = error "undefined simMesh"
    , _simWantedFields = []
    , _simTimesteps = error "undefined simTimesteps"
    , _simSampleGap = 1
    , _simWarmUp = 0
    }

class FDFLParsable a where
  parse :: Parsec String FDFLParseState a

instance FDFLParsable s => FDFLParsable [s] where
  parse = parseBrackets $ parseCommaSep parse

instance FDFLParsable s => FDFLParsable (Maybe s) where
  parse = Just <$> parse

instance (FDFLParsable a, FDFLParsable b) => FDFLParsable (a, b) where
  parse = parseParens $ do
    a <- parse
    _ <- parseComma
    b <- parse
    return (a, b)

instance FDFLParsable (FieldExpr Identifier) where
  parse = expr
    where
    expr = buildExpressionParser table term
    term = choice
      [ parseUnary "grad" FieldGradient
      , parseUnary "div" FieldDivergence
      , parseUnary "laplace" (FieldDivergence . FieldGradient)
      , parseUnary "Dt" FieldTemporalDerivative
      , parseUnary "Dn" FieldNormalDerivative
      , parseUnary "exp" FieldExponent
      , parseUnary "cos" FieldCos
      , parseUnary "sin" FieldSin
      , parseUnary "tan" FieldTan
      , parseBinary "inner" FieldInner
      , parseBinary "outer" FieldOuter
      , parseBinary "dot" FieldDot
      , parseBinary "pow" FieldPower
      , parseUnary "Dx" $ flip FieldSpatialDerivative 0
      , parseUnary "Dy" $ flip FieldSpatialDerivative 1
      , parseUnary "Dz" $ flip FieldSpatialDerivative 2
      , parseReserved "pos" >> return FieldPosition
      , parseReserved "pi" >> return FieldPi
      , FieldLiteral <$> parse
      , FieldRef <$> (parse >>= isFieldLike)
      , parseParens expr
      , FieldTensorElements <$> parseExpressionList
      ]
    parseUnary name constructor =
      parseReserved name >> constructor <$> parseParens parse
    parseBinary name constructor =
      parseReserved name >>
      (uncurry constructor) <$> parseParens parsePair
    parsePair = do
      first <- parse
      _ <- parseComma
      second <- parse
      return (first, second)
    table =
      [ [ Postfix (FieldIndexOperation <$> parseIndices) ]
      , [ Prefix $ parseSymbol "-" >> return negate' ]
      , [ Infix (parseSymbol "*" >> return FieldOuter) AssocLeft
        , Infix (parseSymbol "/" >> return FieldDivision) AssocLeft
        ]
      , [ Infix (parseSymbol "+" >> return FieldAddition) AssocLeft
        , Infix (parseSymbol "-" >> return (\a -> FieldAddition a . negate')) AssocLeft
        ]
      ]
    negate' = FieldOuter . FieldLiteral . ScalarConstant $ (-1)
    parseIndices = parseBrackets $ parseCommaSep parseInteger
    parseExpressionList = parseBrackets $ parseCommaSep expr

parseBoundedEnum :: (Show a, Enum a, Bounded a) => FDFLParser a
parseBoundedEnum = choice $ toParser <$> values
  where
  values = [minBound .. maxBound]
  toParser = \x -> parseReserved (show x) >> return x

instance FDFLParsable Bool where
  parse = parseBoundedEnum

instance FDFLParsable StaggerStrategy where
  parse = parserFailEither $ matchWithEnum <$> parseStringLiteral
    where
    values = [minBound .. maxBound]
    mappings = Map.fromList $ (\e -> (toLower <$> show e, e)) <$> values
    matchWithEnum str = case Map.lookup str mappings of
      Just value -> Right value
      Nothing -> Left $ "Invalid staggering strategy: " ++ str

instance FDFLParsable StringLiteral where
  parse = StringLiteral <$> parseStringLiteral

instance FDFLParsable Identifier where
  parse = Identifier <$> parseIdentifier

instance FDFLParsable Rational where
  parse = choice
    [ try $ parseLexeme parseFloatAsRational
    , fromInteger <$> parseInteger
    ]

parseFloatAsRational :: Stream s m Char => ParsecT s u m Rational
parseFloatAsRational = makeResult <$> choice
  [ try parseA
  , try parseB
  , parseC
  ]
  where
  makeResult :: (Char, String, String, Integer) -> Rational
  makeResult (s, i, f, e) = (signToInteger s) * raised
    where
    base = (fromIntegral $ digitsToInteger i) + (digitsToFrac f)
    raised = base * ((10::Rational) ^^ e)
  parseSign = choice $ char <$> ['+', '-']
  signToInteger sign = case sign of
    '+' -> 1
    '-' -> (-1)
    _ -> error $ "signToInteger: unexpected " ++ show sign
  parseExponent = do
    _ <- choice $ char <$> ['e', 'E']
    sign <- option '+' parseSign
    digits <- many1 digit
    return $ (signToInteger sign) * (digitsToInteger digits)
  parseA = do
    sign <- option '+' parseSign
    intDigits <- many1 digit
    _ <- char '.'
    fracDigits <- many digit
    exp' <- option 0 parseExponent
    return (sign, intDigits, fracDigits, exp')
  parseB = do
    sign <- option '+' parseSign
    _ <- char '.'
    fracDigits <- many1 digit
    exp' <- option 0 parseExponent
    return (sign, [], fracDigits, exp')
  parseC = do
    sign <- option '+' parseSign
    intDigits <- many1 digit
    exp' <- parseExponent
    return (sign, intDigits, [], exp')
  digitsToInteger :: [Char] -> Integer
  digitsToInteger [] = 0
  digitsToInteger (d:ds) = (fromIntegral $ digitToInt d) * ((10::Integer) ^ (length ds)) + digitsToInteger ds
  digitsToFrac :: [Char] -> Rational
  digitsToFrac [] = 0
  digitsToFrac ds = (fromIntegral $ digitsToInteger ds) / ((10::Rational) ^^ (length ds))

instance FDFLParsable Integer where
  parse = parseInteger

instance FDFLParsable LiteralConstant where
  parse = choice
    [ parseNullary "Permutation" PermutationSymbol
    , ScalarConstant <$> parse
    ]
    where
    parseNullary name constructor =
      parseReserved name >> const constructor <$> parseParens spaces

containsSymbol :: FDFL -> Identifier -> Bool
containsSymbol fdfl = isJust . getSymbol fdfl

getSymbol :: FDFL -> Identifier -> Maybe Definition
getSymbol fdfl (Identifier sym) = Map.lookup sym (symbols fdfl)

getFieldDef :: FDFL -> Identifier -> Field
getFieldDef fdfl ident = case getSymbol fdfl ident of
  Just (FieldDef f) -> f
  _ -> error $ "getFieldDef: unknown field " ++ identifierValue ident

addDefinition :: FDFL -> Identifier -> Definition -> Either String FDFL
addDefinition fdfl symName def = if containsSymbol fdfl symName
  then Left $ "Attempt to redefine symbol " ++ identifierValue symName
  else Right fdfl { symbols = Map.insert (identifierValue symName) def $ symbols fdfl}

fdflDef :: LanguageDef st
fdflDef = emptyDef
  { caseSensitive = True
  , reservedOpNames = ["="]
  , reservedNames = ["True", "False"]
  , commentLine = "#"
  , identStart = letter
  }

TokenParser
  { identifier = parseIdentifier
  , reservedOp = parseReservedOp
  , reserved = parseReserved
  , parens = parseParens
  , commaSep = parseCommaSep
  , comma = parseComma
  , stringLiteral = parseStringLiteral
  , integer = parseInteger
  , brackets = parseBrackets
  , symbol = parseSymbol
  , lexeme = parseLexeme
  } = makeTokenParser fdflDef

parseFDFL :: FDFLParser FDFLParseState
parseFDFL =
  many parseAssignment >>
  eof >>
  getState

parserFailEither :: ParsecT s u m (Either String a) -> ParsecT s u m a
parserFailEither p = do
  parsed <- p
  case parsed of
    Right value -> return value
    Left str -> parserFail str

parseAssignment :: FDFLParser ()
parseAssignment = do
  symbolName <- parseIdentifier
  _ <- parseReservedOp "="
  oldState <- getState
  let addSymbol = addDefinition (_psFDFL oldState) (Identifier symbolName)
  let setFDFL = flip (Lens.set psFDFL) oldState
  newState <- setFDFL <$> (parserFailEither $ addSymbol <$> parseDefinition)
  putState newState
  return ()

parseDefinition :: FDFLParser Definition
parseDefinition = choice
  [ parseSpecToParser parseField
  , parseSpecToParser parseMesh
  , parseSpecToParser parseEquation
  , parseSpecToParser parseBoundaryCondition
  , parseSpecToParser parseMeshConstant
  , parseSpecToParser parseSolve
  , parseSpecToParser parseNamedLiteral
  , parseSpecToParser parseSimulation
  , FieldExprDef <$> parse
  ]

parseInput :: String -> String -> Either ParseError FDFL
parseInput sourceName s
  = _psFDFL <$> runParser parseFDFL emptyFDFLParseState sourceName s
