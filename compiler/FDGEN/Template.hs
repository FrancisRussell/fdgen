module FDGEN.Template where
import Control.Applicative ((<$>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.Parsec.Language (emptyDef, LanguageDef)
import Text.Parsec (ParsecT, ParseError, runParser, getState, Parsec, putState, try)
import Text.Parsec.Token (GenTokenParser(..), GenLanguageDef(..), makeTokenParser)
import Text.Parsec.Combinator (eof, choice, many1, manyTill, optionMaybe)
import Text.Parsec.Char (anyChar, char, string, noneOf)
import Text.Parsec.Prim (many, parserFail)

data TemplateElement
  = Text String
  | Lookup String
  deriving Show

data Val
  = StringVal String
  | DictVal Dict
  | ListVal [Val]
  deriving Show

data Dict = Dict {
  _dictMappings :: Map String Val
} deriving Show

TokenParser
  { identifier = parseIdentifier
  , reserved = parseReserved
  } = makeTokenParser emptyDef

data TemplateParseState = TemplateParseState {
  _stateDict :: Dict
}

type TemplateParser = Parsec String TemplateParseState [TemplateElement]
type TemplateElementParser = Parsec String TemplateParseState TemplateElement

buildParseState :: Dict -> TemplateParseState
buildParseState dict = TemplateParseState {
  _stateDict = dict
}

parseTemplate :: TemplateParser
parseTemplate = many $ choice parsers
  where
  parsers = [ parseNonSpecial
            , parseDirective
            ]

parseDirective :: TemplateElementParser
parseDirective = do
  try $ string "${"
  content <- parseDirective'
  string "}"
  return content

parseDirective' :: TemplateElementParser
parseDirective' = do
  Lookup <$> parseIdentifier

parseNonSpecial :: TemplateElementParser
parseNonSpecial = Text <$> (many1 $ noneOf ['$'])

lookupScalar :: Dict -> String -> Either String String
lookupScalar dict key = case Map.lookup key (_dictMappings dict) of
  Just (StringVal s) -> Right s
  Just _ -> Left $ "Key " ++ key ++ " does not map to scalar"
  _ -> Left $ "Key " ++ key ++ " not found"

parserFailEither :: ParsecT s u m (Either String a) -> ParsecT s u m a
parserFailEither p = do
  parsed <- p
  case parsed of
    Right value -> return value
    Left str -> parserFail str

populate :: Dict -> String -> Either ParseError String
populate dict template = show <$> parsedTemplate
  where
  parseState = buildParseState dict
  parsedTemplate = runParser parseTemplate parseState "<template>" template

emptyDict :: Dict
emptyDict = Dict
  {  _dictMappings = Map.empty
  }

insert :: String -> Val -> Dict -> Dict
insert k v dict = dict {
  _dictMappings = Map.insert k v (_dictMappings dict)
}
