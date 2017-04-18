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

apply :: String -> Dict -> String
apply str _ = str

emptyDict :: Dict
emptyDict = Dict
  {  _dictMappings = Map.empty
  }
