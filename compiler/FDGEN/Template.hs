{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}
module FDGEN.Template where
import Control.Applicative ((<$>))
import Control.Monad (foldM)
import Data.Map.Strict (Map)
import qualified Control.Lens as Lens
import qualified Data.Map.Strict as Map
import Text.Parsec.Language (emptyDef, LanguageDef)
import Text.Parsec (ParsecT, runParser, Parsec, try)
import Text.Parsec.Token (GenTokenParser(..), GenLanguageDef(..), makeTokenParser)
import Text.Parsec.Combinator (eof, choice, many1, sepBy1, optional)
import Text.Parsec.Char (letter, string, noneOf, newline)
import Text.Parsec.Prim (many, parserFail)

type Path = [ String ]
type Dict = Map String Val
type Scope = [Dict]

data TemplateElement
  = Text String
  | Lookup Path
  | ForEach String Path [TemplateElement]
  deriving Show

data Val
  = StringVal String
  | DictVal Dict
  | ListVal [Val]
  deriving Show

valToList :: Val -> Either String [Val]
valToList val = case val of
  ListVal list -> Right list
  _ -> Left $ show val ++ " cannot be used as a List"

valToString :: Val -> Either String String
valToString val = case val of
  StringVal str -> Right str
  _ -> Left $ show val ++ " cannot be used as String"

data StringBuilder = StringBuilder
  { _sbResult :: String
  , _sbScope :: Scope
  } deriving Show

Lens.makeLenses ''StringBuilder

class ScopeLike a where
  scopeLookup :: String -> a -> Maybe Val

class NestedScopeLike a where
  pushScope :: Dict -> a -> a
  popScope :: a -> a

instance NestedScopeLike Scope where
  pushScope scope dicts = scope:dicts
  popScope (_:dicts) = dicts
  popScope [] = error "Attempted to pop scope from empty stack."

instance NestedScopeLike StringBuilder where
  pushScope dict = Lens.over sbScope (pushScope dict)
  popScope = Lens.over sbScope popScope

instance ScopeLike Dict where
  scopeLookup = dictLookup

instance ScopeLike Scope where
  scopeLookup binding (d:ds) = case scopeLookup binding d of
    Just val -> Just val
    Nothing -> scopeLookup binding ds
  scopeLookup _ [] = Nothing

instance ScopeLike StringBuilder where
  scopeLookup binding sb = scopeLookup binding $ _sbScope sb

builderAppend :: String -> StringBuilder -> StringBuilder
builderAppend str = Lens.over sbResult (++str)

scopeLookupPath :: ScopeLike s => Path -> s -> Either String Val
scopeLookupPath [] _ = error "Empty path"
scopeLookupPath [final] scope = case scopeLookup final scope of
  Just val -> Right val
  Nothing -> Left $ "Missing binding for " ++ final
scopeLookupPath (p:ps) scope = case scopeLookup p scope of
  Nothing -> Left $ "Missing binding for " ++ p ++ " in path " ++ (show (p:ps))
  Just (DictVal d) -> scopeLookupPath ps d
  Just _ -> Left $ "Sub-path " ++ (show ps) ++ " does not resolve to a scope."

dictLookup :: String -> Dict -> Maybe Val
dictLookup = Map.lookup

templateDef :: LanguageDef st
templateDef = emptyDef
  { caseSensitive = True
  , reservedNames = ["for", "end", "in"]
  , identStart = letter
  }

TokenParser
  { identifier = parseIdentifier
  , reserved = parseReserved
  } = makeTokenParser templateDef

type TemplateParseState = ()
type Template = [TemplateElement]

type TemplateParser = Parsec String TemplateParseState Template
type TemplateElementParser = Parsec String TemplateParseState TemplateElement

parseTemplate :: TemplateParser
parseTemplate = do
  result <- parseBlock
  eof
  return result

parseBlock :: TemplateParser
parseBlock = many $ choice parsers
  where
  parsers = [ parseDirective
            , parseNonSpecial
            ]

parseDirectiveWrapper :: Parsec String TemplateParseState e -> Parsec String TemplateParseState e
parseDirectiveWrapper contentParser = do
  content <- try (string "${" >> contentParser)
  _ <- string "}"
  _ <- optional newline
  return content

parseDirective :: TemplateElementParser
parseDirective = choice
  [ parseFor,
    parseLookup
  ]

parsePath :: Parsec String TemplateParseState Path
parsePath = sepBy1 parseIdentifier $ string "."

parseLookup :: TemplateElementParser
parseLookup = Lookup <$> (parseDirectiveWrapper parsePath)

parseEnd :: Parsec String TemplateParseState ()
parseEnd = parseDirectiveWrapper (parseReserved "end") >> return ()

parseFor :: TemplateElementParser
parseFor = do
  (ident, path) <- parseDirectiveWrapper parseSpecial
  block <- parseBlock
  parseEnd
  return $ ForEach ident path block
  where
  parseSpecial = do
    parseReserved "for"
    ident <- parseIdentifier
    parseReserved "in"
    path <- parsePath
    return (ident, path)

parseNonSpecial :: TemplateElementParser
parseNonSpecial = Text <$> (many1 $ noneOf ['$'])

lookupScalar :: Dict -> String -> Either String String
lookupScalar dict key = case Map.lookup key dict of
  Just (StringVal s) -> Right s
  Just _ -> Left $ "Key " ++ key ++ " does not map to scalar"
  _ -> Left $ "Key " ++ key ++ " not found"

parserFailEither :: ParsecT s u m (Either String a) -> ParsecT s u m a
parserFailEither p = do
  parsed <- p
  case parsed of
    Right value -> return value
    Left str -> parserFail str

populateTemplate :: Dict -> Template -> Either String String
populateTemplate _ [] = Right ""
populateTemplate dict template = _sbResult <$> foldM populateTemplate' initial template
  where
  initial = StringBuilder
    { _sbResult = ""
    , _sbScope = [dict]
    }
  populateTemplate' :: StringBuilder -> TemplateElement -> Either String StringBuilder
  populateTemplate' builder (Text str) = return $ builderAppend str builder
  populateTemplate' builder (Lookup path) = do
    val <- scopeLookupPath path builder
    str <- valToString val
    return $ builderAppend str builder
  populateTemplate' builder (ForEach binding path body) = do
    bindingsVal <- scopeLookupPath path builder
    bindings <- valToList bindingsVal
    foldM (doIteration body) builder bindings
    where
    doIteration :: [TemplateElement] -> StringBuilder -> Val -> Either String StringBuilder
    doIteration elements sb val = popScope <$> sb''
      where
      sb' = pushScope (Map.fromList [(binding, val)]) sb
      sb'' = foldM populateTemplate' sb' elements

populate :: Dict -> String -> Either String String
populate dict template = populatedTemplate
  where
  parseState = ()
  parsedTemplate = runParser parseTemplate parseState "<template>" template
  populatedTemplate = populateTemplate dict =<< (Lens.over Lens._Left show parsedTemplate)

emptyDict :: Dict
emptyDict = Map.empty

insert :: String -> Val -> Dict -> Dict
insert k v dict = Map.insert k v dict
