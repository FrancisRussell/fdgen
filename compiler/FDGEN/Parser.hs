{-# LANGUAGE TemplateHaskell, Rank2Types, FlexibleInstances #-}
module FDGEN.Parser ( parseInput, FDFL, getSymbols, Definition(..)
                    , Mesh(..), stringLiteralValue, identifierValue
                    , getSymbol, Field(..), Solve(..), Equation(..)
                    , FieldExpr(..), getFieldDef, LiteralConstant(..)
                    , Constant(..), Identifier(..), StaggerStrategy(..)
                    ) where
import Data.Char (toLower)
import Data.Map (Map)
import Data.Maybe (isJust)
import Data.List (genericIndex)
import Control.Applicative ((<$>))
import Control.Monad ((>=>))
import Text.Parsec.Char (letter, spaces)
import Text.Parsec.Combinator (eof, choice, optionMaybe)
import Text.Parsec.Language (emptyDef, LanguageDef)
import Text.Parsec (ParsecT, ParseError, runParser, getState, Parsec, putState)
import Text.Parsec.Prim (many, parserFail)
import Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))
import Text.Parsec.Token (GenTokenParser(..), GenLanguageDef(..), makeTokenParser)
import FDGEN.Pretty (PrettyPrintable(..), structureDoc, hListDoc)
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.PrettyPrint as PrettyPrint

data Identifier = Identifier String
  deriving (Show, Ord, Eq)

instance PrettyPrintable Identifier where
  toDoc (Identifier i) = PrettyPrint.text i

data StringLiteral = StringLiteral String
  deriving (Show, Ord, Eq)

instance PrettyPrintable StringLiteral where
  toDoc (StringLiteral s) = PrettyPrint.text $ show s

stringLiteralValue :: StringLiteral -> String
stringLiteralValue (StringLiteral v) = v

identifierValue :: Identifier -> String
identifierValue (Identifier v) = v

data Mesh = Mesh {
  _meshName :: StringLiteral,
  _meshDimension :: Integer,
  _meshFields :: [Identifier],
  _meshSolves :: [Identifier]
} deriving Show

instance PrettyPrintable Mesh
  where
  toDoc mesh = structureDoc "Mesh"
    [ ("name", toDoc $ _meshName mesh)
    , ("dim", toDoc $ _meshDimension mesh)
    , ("fields", hListDoc $ _meshFields mesh)
    , ("solves", hListDoc $ _meshSolves mesh)
    ]

data Field = Field {
  _fieldName :: StringLiteral,
  _fieldRank :: Integer,
  _fieldSymmetric :: Bool,
  _fieldStaggerStrategySpatial :: StaggerStrategy,
  _fieldStaggerStrategyTemporal :: StaggerStrategy
} deriving Show

instance PrettyPrintable Field
  where
  toDoc field = structureDoc "Field"
    [ ("name", toDoc $ _fieldName field)
    , ("rank", toDoc $ _fieldRank field)
    , ("symmetric", toDoc $ _fieldSymmetric field)
    , ("spatial_stagger", toDoc $ _fieldStaggerStrategySpatial field)
    , ("temporal_stagger", toDoc $ _fieldStaggerStrategyTemporal field)
    ]

data FieldExpr a
  = FieldRef a
  | FieldLiteral LiteralConstant
  | FieldAddition (FieldExpr a) (FieldExpr a)
  | FieldDivision (FieldExpr a) (FieldExpr a)
  | FieldInner (FieldExpr a) (FieldExpr a)
  | FieldOuter (FieldExpr a) (FieldExpr a)
  | FieldDot (FieldExpr a) (FieldExpr a)
  | FieldGradient (FieldExpr a)
  | FieldDivergence (FieldExpr a)
  | FieldSpatialDerivative (FieldExpr a) Integer
  | FieldTemporalDerivative (FieldExpr a)
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
    FieldLiteral (ScalarConstant r) -> PrettyPrint.double r
    FieldLiteral PermutationSymbol -> text "epsilon"
    where
      text = PrettyPrint.text
      hcat = PrettyPrint.hcat
      dim x = genericIndex (text <$> ["x", "y", "z"] ++ ["dim(" ++ show n ++ ")" | n <- [3..] :: [Integer]]) x
      binaryOp op a b = hcat [text "(", toDoc a, text $ " " ++ op ++ " ", toDoc b, text ")"]
      function name params = hcat [prefix, content, suffix]
        where
        prefix = text $ name ++ "("
        suffix = text ")"
        content = hcat $ PrettyPrint.punctuate (text ", ") (toDoc <$> params)

data LiteralConstant
  = ScalarConstant Double
  | PermutationSymbol
  deriving Show

data Constant = Constant {
  _constantRank :: Integer,
  _constantName :: StringLiteral
} deriving Show

instance PrettyPrintable Constant
 where
 toDoc constant = structureDoc "Constant"
   [ ("rank", toDoc $ _constantRank constant)
   , ("name", toDoc $ _constantName constant)
   ]

data Equation = Equation {
  _fieldUpdateLHS :: FieldExpr Identifier,
  _fieldUpdateRHS :: FieldExpr Identifier
} deriving Show

instance PrettyPrintable Equation
 where
 toDoc equ = structureDoc "Equation"
   [ ("lhs", toDoc $ _fieldUpdateLHS equ)
   , ("rhs", toDoc $ _fieldUpdateRHS equ)
   ]

data Solve = Solve {
  _solveName :: StringLiteral,
  _solveSpatialOrder :: Integer,
  _solveTemporalOrder :: Integer,
  _solveEquations :: [Identifier]
} deriving Show

instance PrettyPrintable Solve
 where
 toDoc solve = structureDoc "Solve"
   [ ("name", toDoc $ _solveName solve)
   , ("spatial_order", toDoc $ _solveSpatialOrder solve)
   , ("temporal_order", toDoc $ _solveTemporalOrder solve)
   , ("equations", hListDoc $ _solveEquations solve)
   ]

data Definition
 = FieldDef Field
 | MeshDef Mesh
 | EquationDef Equation
 | ConstantDef Constant
 | FieldExprDef (FieldExpr Identifier)
 | SolveDef Solve
 deriving Show

instance PrettyPrintable Definition
 where
 toDoc def = case def of
   FieldDef f -> toDoc f
   MeshDef m -> toDoc m
   EquationDef e -> toDoc e
   ConstantDef c -> toDoc c
   FieldExprDef f -> toDoc  f
   SolveDef s -> toDoc s

data StaggerStrategy
  = All
  | None
  | Dimension
  deriving (Bounded, Enum, Show)

instance PrettyPrintable StaggerStrategy where
  toDoc = PrettyPrint.text . show

data FDFL = FDFL {
  symbols :: Map String Definition
} deriving Show

instance PrettyPrintable FDFL
  where
  toDoc fdfl = structureDoc "FDFL" fields
    where
    fields = fieldDoc <$> (Map.assocs $ symbols fdfl)
    fieldDoc (name, value) = (name, toDoc value)

data FDFLParseState = FDFLParseState {
  _psFDFL :: FDFL
} deriving Show

Lens.makeLenses ''FDFLParseState
Lens.makeLenses ''Mesh
Lens.makeLenses ''Field
Lens.makeLenses ''Equation
Lens.makeLenses ''Constant
Lens.makeLenses ''Solve

emptyFDFL :: FDFL
emptyFDFL = FDFL {
  symbols = Map.empty
}

getSymbols :: FDFL -> Map String Definition
getSymbols = symbols

emptyFDFLParseState :: FDFLParseState
emptyFDFLParseState = FDFLParseState {
  _psFDFL = emptyFDFL
}

type FDFLParser a = Parsec String FDFLParseState a

type Validator a = a -> FDFLParser a

data AttributeUpdate s = AttributeUpdate String (s -> s)

data PositionalSpec s = PositionalSpec String (FDFLParser (AttributeUpdate s))

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
      else parserFail $ identifierValue name ++ "should be of type " ++ friendlyType ++ " but is not."

knownIdentifier :: Validator Identifier
knownIdentifier = validateDefinition (const True) "any"

isFieldLike :: Validator Identifier
isFieldLike = validateDefinition validate "field"
  where validate def = case def of
                         FieldDef _ -> True
                         FieldExprDef _ -> True
                         ConstantDef _ -> True
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
  , buildAttributeSpec "spatial_stagger_strategy" False alwaysValid fieldStaggerStrategySpatial
  , buildAttributeSpec "temporal_stagger_strategy" False alwaysValid fieldStaggerStrategyTemporal
  ]

parseMesh :: ObjectParseSpec Mesh
parseMesh = ObjectParseSpec "Mesh" []
  [ buildAttributeSpec "name" True alwaysValid meshName
  , buildAttributeSpec "dimension" True alwaysValid meshDimension
  , buildAttributeSpec "fields" True (validateList isFieldLike >=> noDuplicates) meshFields
  , buildAttributeSpec "solves" True (validateList isSolve >=> noDuplicates) meshSolves
  ]

parseEquation :: ObjectParseSpec Equation
parseEquation = ObjectParseSpec "Equation"
  [ buildPositionalSpec "lhs" alwaysValid fieldUpdateLHS
  , buildPositionalSpec "rhs" alwaysValid fieldUpdateRHS
  ]
  []

parseConstant :: ObjectParseSpec Constant
parseConstant = ObjectParseSpec "Constant"
  []
  [ buildAttributeSpec "name" True alwaysValid constantName
  , buildAttributeSpec "rank" False alwaysValid constantRank
  ]

parseSolve :: ObjectParseSpec Solve
parseSolve = ObjectParseSpec "Solve"
  []
  [ buildAttributeSpec "name" True alwaysValid solveName
  , buildAttributeSpec "spatial_order" False alwaysValid solveSpatialOrder
  , buildAttributeSpec "temporal_order" False alwaysValid solveTemporalOrder
  , buildAttributeSpec "equations" False (validateList isEquation >=> noDuplicates) solveEquations
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
    }

instance FDFLObject Equation where
  wrapObject = EquationDef
  emptyObject = Equation
    { _fieldUpdateLHS = error "undefined fieldUpdateLHS"
    , _fieldUpdateRHS = error "undefined fieldUpdateRHS"
    }

instance FDFLObject Constant where
  wrapObject = ConstantDef
  emptyObject = Constant
    { _constantName = error "undefined constantName"
    , _constantRank = 0
    }

instance FDFLObject Solve where
  wrapObject = SolveDef
  emptyObject = Solve
    { _solveName = error "undefined solveName"
    , _solveSpatialOrder = 1
    , _solveTemporalOrder = 1
    , _solveEquations = []
    }

class FDFLParsable a where
  parse :: Parsec String FDFLParseState a

instance FDFLParsable s => FDFLParsable [s] where
  parse = parseBrackets $ parseCommaSep parse

instance FDFLParsable (FieldExpr Identifier) where
  parse = expr
    where
    expr = buildExpressionParser table term
    term = choice
      [ parseUnary "grad" FieldGradient
      , parseUnary "div" FieldDivergence
      , parseUnary "laplace" (FieldDivergence . FieldGradient)
      , parseUnary "Dt" FieldTemporalDerivative
      , parseBinary "inner" FieldInner
      , parseBinary "outer" FieldOuter
      , parseBinary "dot" FieldDot
      , parseUnary "Dx" $ flip FieldSpatialDerivative 0
      , parseUnary "Dy" $ flip FieldSpatialDerivative 1
      , parseUnary "Dz" $ flip FieldSpatialDerivative 2
      , FieldLiteral <$> parse
      , FieldRef <$> (parse >>= isFieldLike)
      , parseParens expr
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
      [ [ Prefix $ parseSymbol "-" >> return negate' ]
      , [ Infix (parseSymbol "*" >> return FieldOuter) AssocLeft
        , Infix (parseSymbol "/" >> return FieldDivision) AssocLeft
        ]
      , [ Infix (parseSymbol "+" >> return FieldAddition) AssocLeft
        , Infix (parseSymbol "-" >> return (\a -> FieldAddition a . negate')) AssocLeft
        ]
      ]
    negate' = FieldOuter . FieldLiteral . ScalarConstant $ -1.0

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

instance FDFLParsable Integer where
  parse = parseInteger

instance FDFLParsable LiteralConstant where
  parse = choice [ parseNullary "Permutation" PermutationSymbol
          , ScalarConstant <$> parseFloat
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
  , float = parseFloat
  , symbol = parseSymbol
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
  , parseSpecToParser parseConstant
  , parseSpecToParser parseSolve
  , FieldExprDef <$> parse
  ]

parseInput :: String -> String -> Either ParseError FDFL
parseInput sourceName s
  = _psFDFL <$> runParser parseFDFL emptyFDFLParseState sourceName s
