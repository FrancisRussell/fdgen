module FDGEN.Parser (parseInput) where
import Data.Map
import Data.Set
import Control.Applicative ((<$>))
import Control.Monad (foldM)
import Text.Parsec.Char (letter)
import Text.Parsec.Combinator (eof, choice)
import Text.Parsec.Language (emptyDef, LanguageDef)
import Text.Parsec (ParsecT, ParseError, runParser, getState, Parsec, putState)
import Text.Parsec.Prim (many, parserFail, (<|>), parsecMap)
import Text.Parsec.Token (GenTokenParser(..), GenLanguageDef(..), makeTokenParser)
import qualified Data.Map as Map
import qualified Data.Set as Set

data FDFL = FDFL {
  symbols :: Map String Definition
} deriving (Show)

emptyFDFL :: FDFL
emptyFDFL = FDFL {
  symbols = Map.empty
}

type FDFLParser a = Parsec String FDFL a

data ObjectType
 = FieldType

data Field = Field {
  fieldName :: String,
  fieldRank :: Integer
} deriving Show

data Mesh = Mesh {
  meshName :: String,
  meshDimension :: Integer,
  meshFields :: [String]
} deriving Show

data MeshUpdate = MeshUpdate String [FieldUpdate]
  deriving Show

data FieldUpdate = FieldUpdate String FieldExpr
  deriving Show

data FieldExpr = FieldRef String
  deriving Show

data Definition
 = FieldDef Field
 | MeshDef Mesh
 | FieldUpdateDef FieldUpdate
 deriving Show

data AttributeValue
 = StringAttribute String
 | IdentifierAttribute String
 | IntegerAttribute Integer
 | IdentifierListAttribute [String]
 | BooleanAttribute Bool

toStringAttribute :: AttributeValue -> Maybe String
toStringAttribute (StringAttribute str) = Just str
toStringAttribute _ = Nothing

toIntegerAttribute :: AttributeValue -> Maybe Integer
toIntegerAttribute (IntegerAttribute i) = Just i
toIntegerAttribute _ = Nothing

toBooleanAttribute :: AttributeValue -> Maybe Bool
toBooleanAttribute (BooleanAttribute i) = Just i
toBooleanAttribute _ = Nothing

toIdentifierAttribute :: AttributeValue -> Maybe String
toIdentifierAttribute (IdentifierAttribute i) = Just i
toIdentifierAttribute _ = Nothing

toIdentifierListAttribute :: AttributeValue -> Maybe [String]
toIdentifierListAttribute (IdentifierListAttribute l) = Just l
toIdentifierListAttribute _ = Nothing

containsSymbol :: FDFL -> String -> Bool
containsSymbol fdfl sym = Map.member sym $ symbols fdfl

getTypedAttribute :: (AttributeValue -> Maybe v) -> Map String AttributeValue -> String -> Either String v
getTypedAttribute converter mapping key = case Map.lookup key mapping of
  Just value -> case converter value of
    Just typedValue -> Right typedValue
    Nothing -> Left $ "Attribute " ++ show key ++ " has incorrect type"
  Nothing ->  Left $ "Attribute " ++ show key ++ " not found"

getStringAttribute :: Map String AttributeValue -> String -> Either String String
getStringAttribute = getTypedAttribute toStringAttribute

getIntegerAttribute :: Map String AttributeValue -> String -> Either String Integer
getIntegerAttribute = getTypedAttribute toIntegerAttribute

getBooleanAttribute :: Map String AttributeValue -> String -> Either String Bool
getBooleanAttribute = getTypedAttribute toBooleanAttribute

getIdentifierAttribute :: Map String AttributeValue -> String -> Either String String
getIdentifierAttribute = getTypedAttribute toIdentifierAttribute

getIdentifierListAttribute :: Map String AttributeValue -> String -> Either String [String]
getIdentifierListAttribute = getTypedAttribute toIdentifierListAttribute

getFieldIdentifier :: FDFL -> String -> Either String String
getFieldIdentifier fdfl ident = case Map.lookup ident $ symbols fdfl of
  Nothing -> Left $ "Symbol " ++ show ident ++ " not defined."
  Just sym -> case sym of
    FieldDef _ -> Right ident
    _ -> Left $ "Expected a field, but " ++ ident ++ " is not."


addDefinition :: FDFL -> String -> Definition ->Either String FDFL
addDefinition fdfl symName def = if containsSymbol fdfl symName
  then Left $ "Attempt to redefine symbol " ++ symName
  else Right fdfl { symbols = Map.insert symName def $ symbols fdfl}

fdflDef :: LanguageDef st
fdflDef = emptyDef {
  caseSensitive = True,
  reservedOpNames = ["="],
  reservedNames = ["Field", "True", "False", "Mesh", "FieldUpdate"],
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

parseFDFL :: FDFLParser FDFL
parseFDFL =
  many parseAssignment >>
  eof >>
  getState

listToMap :: (Show k, Ord k) => [(k, v)] -> Either String (Map k v)
listToMap lst = foldM combine Map.empty lst
  where
  combine oldMap (newK, newV) = case insertLookupWithKey (\_ v _ -> v) newK newV oldMap of
    (Nothing, newMap) -> Right newMap
    (Just _, _) -> Left $ "Duplicate attribute " ++ show newK

listToSet :: (Show k, Ord k) => [k] -> Either String (Set  k)
listToSet lst = foldM combine Set.empty lst
  where
  combine set newK = if Set.member newK set
    then Left $ "Duplicate identifier " ++ show newK
    else Right $ Set.insert newK set

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
  state <- getState
  newState <- parserFailEither $ addDefinition state symbolName <$> parseDefinition
  putState newState
  return ()

parseDefinition :: FDFLParser Definition
parseDefinition = FieldDef <$> parseFieldDefinition <|>
                  MeshDef <$> parseMeshDefinition <|>
                  FieldUpdateDef <$> parseFieldUpdateDefinition

parseFieldDefinition :: FDFLParser Field
parseFieldDefinition = do
  parseReserved "Field"
  attributeMap <- parserFailEither . parsecMap listToMap . parseParens $ parseCommaSep parseAttribute
  case attributesToField attributeMap of
    Left str -> parserFail str
    Right field -> return field

parseMeshDefinition :: FDFLParser Mesh
parseMeshDefinition = do
  parseReserved "Mesh"
  attributeMap <- parserFailEither . parsecMap listToMap . parseParens $ parseCommaSep parseAttribute
  fdfl <- getState
  case attributesToMesh fdfl attributeMap of
    Left str -> parserFail str
    Right mesh -> return mesh

parseFieldUpdateDefinition :: FDFLParser FieldUpdate
parseFieldUpdateDefinition = do
  parseReserved "FieldUpdate"
  (name, value) <- parseParens parseArguments
  return $ FieldUpdate name value
  where
    parseArguments = do
      fdfl <- getState
      field <- parserFailEither $ getFieldIdentifier fdfl <$> parseIdentifier
      _ <- parseComma
      value <- parseFieldExpr
      return (field, value)

parseFieldExpr :: FDFLParser FieldExpr
parseFieldExpr = do
  fdfl <- getState
  FieldRef <$> (parserFailEither $ getFieldIdentifier fdfl <$> parseIdentifier)

attributesToField :: Map String AttributeValue -> Either String Field
attributesToField _map = do
  name <- getStringAttribute _map "name"
  rank <- getIntegerAttribute _map "rank"
  return Field { fieldName = name, fieldRank = rank }

attributesToMesh :: FDFL -> Map String AttributeValue -> Either String Mesh
attributesToMesh fdfl _map = do
  name <- getStringAttribute _map "name"
  dim <- getIntegerAttribute _map "dimension"
  fields <- (getIdentifierListAttribute _map "fields") >>= mapM (getFieldIdentifier fdfl) >>= listToSet
  return Mesh {meshName = name, meshDimension = dim, meshFields = Set.toList fields}

parseAttribute :: FDFLParser (String, AttributeValue)
parseAttribute = do
  name <- parseIdentifier
  _ <- parseReservedOp "="
  value <- parseAttributeValue
  return (name, value)

parseAttributeValue :: FDFLParser AttributeValue
parseAttributeValue = choice
  [ IdentifierAttribute <$> parseIdentifier
  , StringAttribute <$> parseStringLiteral
  , IntegerAttribute <$> parseInteger
  , BooleanAttribute <$> parseBool
  , IdentifierListAttribute <$> (parseBrackets $ parseCommaSep parseIdentifier)
  ]

parseBool :: FDFLParser Bool
parseBool = choice
  [ parseReserved "True" >> return True
  , parseReserved "False" >> return False
  ]

parseInput :: String -> String -> Either ParseError FDFL
parseInput = runParser parseFDFL emptyFDFL
