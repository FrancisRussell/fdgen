{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module FDGEN.Parser (parseInput) where
import Data.Map (Map)
import Control.Applicative ((<$>))
import Text.Parsec.Char (letter)
import Text.Parsec.Combinator (eof, choice, optionMaybe)
import Text.Parsec.Language (emptyDef, LanguageDef)
import Text.Parsec (ParsecT, ParseError, runParser, getState, Parsec, putState)
import Text.Parsec.Prim (many, parserFail)
import Text.Parsec.Token (GenTokenParser(..), GenLanguageDef(..), makeTokenParser)
import qualified Control.Lens as Lens
import qualified Data.Map as Map

data Identifier = Identifier String
  deriving Show

data StringLiteral = StringLiteral String
  deriving Show

data Mesh = Mesh {
  _meshName :: StringLiteral,
  _meshDimension :: Integer,
  _meshFields :: [Identifier]
} deriving Show

data Field = Field {
  _fieldName :: StringLiteral,
  _fieldRank :: Integer,
  _fieldSymmetric :: Bool,
  _fieldStaggerStrategySpatial :: StaggerStrategy,
  _fieldStaggerStrategyTemporal :: StaggerStrategy
} deriving Show

data FieldUpdate = FieldUpdate {
  _fieldUpdateLHS :: Identifier,
  _fieldUpdateRHS :: Identifier
} deriving Show

data FieldExpr = FieldRef String
  deriving Show

data Definition
 = FieldDef Field
 | MeshDef Mesh
 | FieldUpdateDef FieldUpdate
 deriving Show

data StaggerStrategy
  = All
  | None
  | Dimension
  deriving (Bounded, Enum, Show)

data FDFL = FDFL {
  symbols :: Map String Definition
} deriving Show

data FDFLParseState = FDFLParseState {
  _psFDFL :: FDFL
} deriving Show

Lens.makeLenses ''FDFLParseState
Lens.makeLenses ''Mesh
Lens.makeLenses ''Field
Lens.makeLenses ''FieldUpdate

emptyFDFL :: FDFL
emptyFDFL = FDFL {
  symbols = Map.empty
}

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

knownIdentifier :: Validator Identifier
knownIdentifier (Identifier name) = do
  state <- getState
  let fdfl = _psFDFL state
  if containsSymbol fdfl name
    then return $ Identifier name
    else parserFail $ "Unknown identifer " ++ name

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
  , buildAttributeSpec "fields" True (validateList knownIdentifier) meshFields
  ]

parseFieldUpdate :: ObjectParseSpec FieldUpdate
parseFieldUpdate = ObjectParseSpec "FieldUpdate"
  [ buildPositionalSpec "lhs" knownIdentifier fieldUpdateLHS
  , buildPositionalSpec "rhs" knownIdentifier fieldUpdateRHS
  ]
  []

validateAttributes :: [AttributeSpec s] -> [AttributeUpdate s]
  -> Either String [AttributeUpdate s]
validateAttributes specs updates = if not $ null duplicateAttributes
  then Left $ "Duplicate attribute " ++ head duplicateAttributes
  else if not $ null missing
    then Left $ "Missing attribute " ++ head missing
    else Right updates
  where
  reqAttributes = [name | AttributeSpec name req _ <- specs, req]
  missing = [name | name <- reqAttributes, not $ Map.member name present]
  insertWith' m (k, a) = Map.insertWith (+) k a m
  attributeNames = [name | AttributeUpdate name _ <- updates]
  present = foldl insertWith' Map.empty  [(name, 1::Integer) | name <- attributeNames]
  duplicateAttributes = Map.keys $ Map.filter (> 1) present

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
  emptyObject = Field {
      _fieldSymmetric = False,
      _fieldName = error "undefined fieldName",
      _fieldRank = error "undefined fieldRank",
      _fieldStaggerStrategySpatial = None,
      _fieldStaggerStrategyTemporal = None
    }

instance FDFLObject Mesh where
  wrapObject = MeshDef
  emptyObject = Mesh {
      _meshName = error "undefined meshName",
      _meshDimension = error "undefined meshDimension",
      _meshFields = []
    }

instance FDFLObject FieldUpdate where
  wrapObject = FieldUpdateDef
  emptyObject = FieldUpdate {
    _fieldUpdateLHS = error "undefined fieldUpdateLHS",
    _fieldUpdateRHS = error "undefined fieldUpdateRHS"
  }

class FDFLParsable a where
  parse :: Parsec String FDFLParseState a

instance FDFLParsable s => FDFLParsable [s] where
  parse = parseBrackets $ parseCommaSep parse

parseBoundedEnum :: (Show a, Enum a, Bounded a) => FDFLParser a
parseBoundedEnum = choice $ toParser <$> values
  where
  values = [minBound .. maxBound]
  toParser = \x -> parseReserved (show x) >> return x

instance FDFLParsable Bool where
  parse = parseBoundedEnum

instance FDFLParsable StaggerStrategy where
  parse = parseBoundedEnum

instance FDFLParsable StringLiteral where
  parse = StringLiteral <$> parseStringLiteral

instance FDFLParsable Identifier where
  parse = Identifier <$> parseIdentifier

instance FDFLParsable Integer where
  parse = parseInteger

--data MeshUpdate = MeshUpdate String [FieldUpdate]
--  deriving Show
--

containsSymbol :: FDFL -> String -> Bool
containsSymbol fdfl sym = Map.member sym $ symbols fdfl

addDefinition :: FDFL -> String -> Definition -> Either String FDFL
addDefinition fdfl symName def = if containsSymbol fdfl symName
  then Left $ "Attempt to redefine symbol " ++ symName
  else Right fdfl { symbols = Map.insert symName def $ symbols fdfl}

fdflDef :: LanguageDef st
fdflDef = emptyDef {
  caseSensitive = True,
  reservedOpNames = ["="],
  reservedNames = ["True", "False"],
  commentLine = "#",
  identStart = letter
}

TokenParser {
  identifier = parseIdentifier,
  reservedOp = parseReservedOp,
  reserved = parseReserved,
  parens = parseParens,
  commaSep = parseCommaSep,
  comma = parseComma,
  stringLiteral = parseStringLiteral,
  integer = parseInteger,
  brackets = parseBrackets
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
  let addSymbol = addDefinition (_psFDFL oldState) symbolName
  let setFDFL = flip (Lens.set psFDFL) oldState
  newState <- setFDFL <$> (parserFailEither $ addSymbol <$> parseDefinition)
  putState newState
  return ()

parseDefinition :: FDFLParser Definition
parseDefinition = choice
  [ parseSpecToParser parseField
  , parseSpecToParser parseMesh
  , parseSpecToParser parseFieldUpdate
  ]

parseInput :: String -> String -> Either ParseError FDFL
parseInput sourceName s
  = _psFDFL <$> runParser parseFDFL emptyFDFLParseState sourceName s
