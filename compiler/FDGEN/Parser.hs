module FDGEN.Parser (parseInput) where
import Data.Map
import Control.Applicative ((<$>))
import Text.Parsec.Char (letter)
import Text.Parsec.Combinator (eof, choice)
import Text.Parsec.Language (emptyDef, LanguageDef)
import Text.Parsec (ParseError, runParser, getState, Parsec, modifyState)
import Text.Parsec.Prim (many, parserFail, (<|>))
import Text.Parsec.Token (GenTokenParser(..), GenLanguageDef(..), makeTokenParser)
import qualified Data.Map as Map

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

data Mesh
 = Mesh
 deriving Show

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
  attributes <- parseParens $ parseCommaSep parseAttribute
  case attributesToField attributes of
    Left str -> parserFail str
    Right field -> return field

parseMeshDefinition :: FDFLParser Mesh
parseMeshDefinition = do
  parseReserved "Mesh"
  attributes <- parseParens $ parseCommaSep $ parseAttribute
  case attributesToMesh attributes of
    Left str -> parserFail str
    Right mesh -> return mesh

attributesToField :: [(String, AttributeValue)] -> Either String Field
attributesToField _ = Right Field

attributesToMesh :: [(String, AttributeValue)] -> Either String Mesh
attributesToMesh _ = Right Mesh

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
