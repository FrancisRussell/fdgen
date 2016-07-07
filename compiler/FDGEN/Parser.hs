module FDGEN.Parser (parseInput) where
import Data.Map
import Data.Set
import Control.Applicative ((<$>))
import Control.Monad (foldM)
import Text.Parsec.Char (letter)
import Text.Parsec.Combinator (eof, choice)
import Text.Parsec.Language (emptyDef, LanguageDef)
import Text.Parsec (ParsecT, ParseError, runParser, getState, Parsec, modifyState)
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

data Field
 = Field
 deriving Show

data Mesh = Mesh {
  meshName :: String,
  meshDimension :: Integer,
  meshFields :: [String]
} deriving Show

data Definition
 = FieldDef Field
 | MeshDef Mesh
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
getTypedAttribute converter map key = case Map.lookup key map of
  Just value -> case converter value of
    Just typedValue -> Right typedValue
    Nothing -> Left $ "Attribute " ++ (show key) ++ " has incorrect type"
  Nothing ->  Left $ "Attribute " ++ (show key) ++ " not found"

getStringAttribute :: Map String AttributeValue -> String -> Either String String
getStringAttribute = getTypedAttribute toStringAttribute

getIntegerAttribute :: Map String AttributeValue -> String -> Either String Integer
getIntegerAttribute = getTypedAttribute toIntegerAttribute

getBooleanAttribute :: Map String AttributeValue -> String -> Either String Bool
getBooleanAttribute = getTypedAttribute toBooleanAttribute

getIdentifierAttribute :: Map String AttributeValue -> String -> Either String String
getIdentifierAttribute = getTypedAttribute toIdentifierAttribute

getIdentiferListAttribute :: Map String AttributeValue -> String -> Either String [String]
getIdentiferListAttribute = getTypedAttribute toIdentifierListAttribute

getFieldIdentifier :: FDFL -> String -> Either String String
getFieldIdentifier fdfl id = case Map.lookup id $ symbols fdfl of
  Nothing -> Left $ "Symbol " ++ (show id) ++ " not defined."
  Just sym -> case sym of
    FieldDef _ -> Right id
    _ -> Left $ "Expected a field, but " ++ id ++ " is not."


duplicateSymbolFail :: Show k => k -> a -> a -> a
duplicateSymbolFail k _ _  = error $ "Attempted to redefine symbol " ++ (show k)

addDefinition :: String -> Definition -> FDFL -> FDFL
addDefinition symName def fdfl = fdfl { symbols = Map.insertWithKey duplicateSymbolFail symName def $ symbols fdfl}

fdflDef :: LanguageDef st
fdflDef = emptyDef {
  caseSensitive = True,
  reservedOpNames = ["="],
  reservedNames = ["Field", "True", "False", "Mesh"],
  commentLine = "#",
  identStart = letter
}

TokenParser {
  identifier = parseIdentifier,
  reservedOp = parseReservedOp,
  reserved = parseReserved,
  parens = parseParens,
  commaSep = parseCommaSep,
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
  combine map (newK, newV) = case insertLookupWithKey (\_ v _ -> v) newK newV map of
    (Nothing, newMap) -> Right newMap
    (Just _, _) -> Left $ "Duplicate attribute " ++ (show newK)

listToSet :: (Show k, Ord k) => [k] -> Either String (Set  k)
listToSet lst = foldM combine Set.empty lst
  where
  combine set newK = if Set.member newK set
    then Left $ "Duplicate identifier " ++ (show newK)
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
  definition <- parseDefinition
  modifyState $ addDefinition symbolName definition
  return ()

parseDefinition :: FDFLParser Definition
parseDefinition = FieldDef <$> parseFieldDefinition <|>
                  MeshDef <$> parseMeshDefinition

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

attributesToField :: Map String AttributeValue -> Either String Field
attributesToField _ = Right Field

attributesToMesh :: FDFL -> Map String AttributeValue -> Either String Mesh
attributesToMesh fdfl _map = do
  name <- getStringAttribute _map "name"
  dim <- getIntegerAttribute _map "dimension"
  fields <- (getIdentiferListAttribute _map "fields") >>= mapM (getFieldIdentifier fdfl) >>= listToSet
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

parseInput :: String -> Either ParseError FDFL
parseInput = runParser parseFDFL emptyFDFL ""
