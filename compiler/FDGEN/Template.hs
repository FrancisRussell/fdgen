module FDGEN.Template where
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Val
  = StringVal String
  | DictVal Dict
  | ListVal [Val]
  deriving Show

data Dict = Dict {
  _dictMappings :: Map String Val
} deriving Show

lookup :: String -> Dict -> String
lookup key dict = case Map.lookup key (_dictMappings dict) of
  Just (StringVal s) -> s
  _ -> error "Unknown key"

apply :: Dict -> String -> String
apply _ [] = []
apply dict ('$':remainder) = processEscape dict remainder
apply dict (c:remainder) = c:(apply dict remainder)

processEscape :: Dict -> String -> String
processEscape dict ('$':remainder) = '$':(apply dict remainder)
processEscape dict ('{':remainder) = (FDGEN.Template.lookup name dict) ++ apply dict left
  where
  (name, left) = getContent '}' remainder
processEscape dict str = apply dict str

getContent :: Char -> String -> (String, String)
getContent terminator str = (content, remainder)
  where
  (content, remainderWithTerminator) = break (\x -> x == terminator) str
  remainder = tail remainderWithTerminator

emptyDict :: Dict
emptyDict = Dict
  {  _dictMappings = Map.empty
  }
