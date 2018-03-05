module FDGEN.FPGADSLBackend (FPGADSLBackend(..)) where
import FDGEN.Backend(Backend(..))
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import FDGEN.Discrete (Discretised, buildTemplateDictionary)
import qualified FDGEN.Template as Template

data FPGADSLBackend = FPGADSLBackend

instance Backend FPGADSLBackend
  where
  processDiscretised _ discreteForm = do
    template <- readFile "./templates/jamie_dsl.template"
    case Template.populate (buildTemplateDictionary discreteForm) template of
      Left err -> hPutStrLn stderr (show err) >> exitFailure
      Right generated -> putStrLn generated
