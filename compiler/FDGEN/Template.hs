module FDGEN.Template where
import Control.Applicative ((<$>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.Parsec.Language (emptyDef, LanguageDef)
import Text.Parsec (ParsecT, ParseError, runParser, getState, Parsec, putState, try)
import Text.Parsec.Token (GenTokenParser(..), GenLanguageDef(..), makeTokenParser)
import Text.Parsec.Combinator (eof, choice, optionMaybe)
import Text.Parsec.Char (anyChar, char, string, noneOf)
import Text.Parsec.Prim (many, parserFail)

data Val
  = StringVal String
  | DictVal Dict
  | ListVal [Val]
  deriving Show

data Dict = Dict {
  _dictMappings :: Map String Val
} deriving Show

TokenParser {
} = makeTokenParser emptyDef

data TemplateParseState = TemplateParseState {
  _stateDict :: Dict
}

type TemplateParser a = Parsec String TemplateParseState a

buildParseState :: Dict -> TemplateParseState
buildParseState dict = TemplateParseState {
  _stateDict = dict
}

parseTemplate :: TemplateParser String
parseTemplate = concat <$> (many $ choice parsers)
  where
  parsers = [ parseLookupDirective
            , parseExpressionDirective
            , parseCharacter
            ]

parseCharacter :: TemplateParser String
parseCharacter = return <$> anyChar

parseLookupDirective :: TemplateParser String
parseLookupDirective = do
  try $ string "${"
  directive <- many $ noneOf "}"
  string "}"
  state <- getState
  parserFailEither <$> return $ lookupScalar directive (_stateDict state)

parseExpressionDirective :: TemplateParser String
parseExpressionDirective = do
  try $ string "$("
  directive <- parseExpression
  string ")"
  state <- getState
  parserFailEither <$> return $ lookupScalar directive (_stateDict state)

parseExpression :: TemplateParser String
parseExpression = parseFor

parseFor :: TemplateParser String
parseFor = parserFail "for parsing is unimplemented"

lookupScalar :: String -> Dict -> Either String String
lookupScalar key dict = case Map.lookup key (_dictMappings dict) of
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
populate dict template = runParser parseTemplate parseState "<template>" template
  where
  parseState = buildParseState dict

processDirective :: Dict -> String -> String
processDirective dict str = error "unimplemented directive"

emptyDict :: Dict
emptyDict = Dict
  {  _dictMappings = Map.empty
  }

insert :: String -> Val -> Dict -> Dict
insert k v dict = dict {
  _dictMappings = Map.insert k v (_dictMappings dict)
}
