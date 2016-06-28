module Main (main) where
import System.Environment (getArgs)
import FDGEN.Parser (parseInput)

main :: IO()
main = getArgs >>= processFile . getFileName

getFileName :: [String] -> String
getFileName [filename] = filename
getFileName _ = error "Usage: fgden input_file"

processFile :: String -> IO()
processFile filename = do
  contents <- readFile filename
  let result = parseInput contents in
    case result of
      Left err -> putStrLn $ show err
      Right spec -> putStrLn $ show spec
