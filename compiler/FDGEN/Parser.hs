{-# LANGUAGE TemplateHaskell, Rank2Types, FlexibleInstances #-}
module FDGEN.Parser (parseInput) where
import Data.Map (Map)
import Data.Maybe (isJust)
import Control.Applicative ((<$>))
import Control.Monad ((>=>))
import Text.Parsec.Char (letter)
import Text.Parsec.Combinator (eof, choice, optionMaybe)
import Text.Parsec.Language (emptyDef, LanguageDef)
import Text.Parsec (ParsecT, ParseError, runParser, getState, Parsec, putState)
import Text.Parsec.Prim (many, parserFail)
import Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))
import Text.Parsec.Token (GenTokenParser(..), GenLanguageDef(..), makeTokenParser)
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Set as Set

data Identifier = Identifier String
  deriving (Show, Ord, Eq)

data StringLiteral = StringLiteral String
  deriving (Show, Ord, Eq)

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

data FieldExpr a
  = FieldRef a
  | FieldScalarConstant Double
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

data FieldUpdate = FieldUpdate {
  _fieldUpdateLHS :: FieldExpr Identifier,
  _fieldUpdateRHS :: FieldExpr Identifier
} deriving Show

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

knownIdentifier :: Validator Identifier
knownIdentifier (Identifier name) = do
  state <- getState
  let fdfl = _psFDFL state
  if containsSymbol fdfl name
    then return $ Identifier name
    else parserFail $ "Unknown identifer " ++ name

isField :: Validator Identifier
isField ident = do
  state <- getState
  let fdfl = _psFDFL state
  (Identifier name) <- knownIdentifier ident
  case getSymbol fdfl name of
    Just (FieldDef _) -> return ident
    _ -> parserFail $ name ++ " should be a field, but is not."

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
  , buildAttributeSpec "fields" True (validateList isField >=> noDuplicates) meshFields
  ]

parseFieldUpdate :: ObjectParseSpec FieldUpdate
parseFieldUpdate = ObjectParseSpec "FieldUpdate"
  [ buildPositionalSpec "lhs" alwaysValid fieldUpdateLHS
  , buildPositionalSpec "rhs" alwaysValid fieldUpdateRHS
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

instance FDFLParsable (FieldExpr Identifier) where
  parse = expr
    where expr = buildExpressionParser table term
          term = choice [ FieldScalarConstant <$> parseFloat
                 , parseUnary "grad" FieldGradient
                 , parseUnary "div" FieldDivergence
                 , parseUnary "Dt" FieldTemporalDerivative
                 , parseBinary "inner" FieldInner
                 , parseBinary "outer" FieldOuter
                 , parseBinary "dot" FieldDot
                 , parseUnary "Dx" $ flip FieldSpatialDerivative 0
                 , parseUnary "Dy" $ flip FieldSpatialDerivative 1
                 , parseUnary "Dz" $ flip FieldSpatialDerivative 2
                 , FieldRef <$> (parse >>= isField)
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
          table = [ [ Prefix $ parseSymbol "-" >> return negate' ]
                  , [ Infix (parseSymbol "*" >> return FieldOuter) AssocLeft
                    , Infix (parseSymbol "/" >> return FieldDivision) AssocLeft
                    ]
                  , [ Infix (parseSymbol "+" >> return FieldAddition) AssocLeft
                    , Infix (parseSymbol "-" >> return (\a -> FieldAddition a . negate')) AssocLeft
                    ]
                  ]
          negate' = FieldOuter $ FieldScalarConstant (-1.0)

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
containsSymbol fdfl = isJust . getSymbol fdfl

getSymbol :: FDFL -> String -> Maybe Definition
getSymbol fdfl sym = Map.lookup sym (symbols fdfl)

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
  brackets = parseBrackets,
  float = parseFloat,
  symbol = parseSymbol
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
