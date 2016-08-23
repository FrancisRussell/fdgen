module Main (main) where
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import FDGEN.Parser (parseInput)
import FDGEN.Algebra ()

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
      Right spec -> putStrLn $ show spec
