module FDGEN.Parser (parseInput) where
import Text.Parsec (ParseError, runParser, getState, Parsec)
import Text.Parsec.Language (emptyDef, LanguageDef)
import Text.Parsec.Token (GenTokenParser(..), GenLanguageDef(..), makeTokenParser)
import Data.Set (Set, empty)

data FDFL = FDFL {
  symbols :: Set String
} deriving (Show)

emptyFDFL :: FDFL
emptyFDFL = FDFL {
  symbols = empty
}

type FDFLParser a = Parsec String FDFL a

fdflDef :: LanguageDef st
fdflDef = emptyDef {
  caseSensitive = True,
  reservedOpNames = ["="]
}

TokenParser {
  identifier = parseIdentifier,
  reservedOp = parseReservedOp
} = makeTokenParser fdflDef

parseFDFL :: FDFLParser FDFL
parseFDFL =
  parseAssignment >>
  getState

parseAssignment :: FDFLParser ()
parseAssignment = do
  symbol <- parseIdentifier
  _ <- parseReservedOp "="
  return ()


parseInput :: String -> Either ParseError FDFL
parseInput = runParser parseFDFL emptyFDFL ""
