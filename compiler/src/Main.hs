module Main (main) where
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import FDGEN.Parser (parseInput)
import FDGEN.Discrete (buildDiscreteForm, buildTemplateDictionary)
import FDGEN.Pretty (prettyPrint)
import qualified FDGEN.Template as Template

main :: IO()
main = getArgs >>= processFile . getFileName

getFileName :: [String] -> String
getFileName [filename] = filename
getFileName _ = error "Usage: fgden input_file"

processFile :: String -> IO()
processFile filename = do
  contents <- readFile filename
  template <- readFile "./templates/mesh.hpp.template"
  let result = parseInput filename contents in
    case result of
      Left err -> hPutStrLn stderr (show err) >> exitFailure
      Right spec -> (putStrLn $ prettyPrint spec) >>
                    putStrLn "" >>
                    (putStrLn . prettyPrint $ discreteForm) >>
                    (putStrLn $ Template.apply (buildTemplateDictionary discreteForm) template)
                    where
                    discreteForm = buildDiscreteForm spec
