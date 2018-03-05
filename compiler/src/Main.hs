module Main (main) where
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import FDGEN.Parser (parseInput)
import FDGEN.Discrete (buildDiscreteForm)
import FDGEN.Pretty (prettyPrint)
import FDGEN.Backend(Backend(..))
import FDGEN.CppBackend (CppBackend(..))
import FDGEN.FPGADSLBackend (FPGADSLBackend(..))

main :: IO()
main = getArgs >>= processFile . getFileName

getFileName :: [String] -> String
getFileName [filename] = filename
getFileName _ = error "Usage: fgden input_file"

processFile :: String -> IO()
processFile filename = do
  contents <- readFile filename
  let result = parseInput filename contents in
    case result of
      Left err -> hPutStrLn stderr (show err) >> exitFailure
      Right spec -> (putStrLn $ prettyPrint spec) >>
                    processDiscretised CppBackend discreteForm >>
                    processDiscretised FPGADSLBackend discreteForm
                    where
                    discreteForm = buildDiscreteForm spec
