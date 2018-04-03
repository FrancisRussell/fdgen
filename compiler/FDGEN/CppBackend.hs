module FDGEN.CppBackend (CppBackend(..)) where
import FDGEN.Backend(Backend(..))
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import FDGEN.Discrete (buildTemplateDictionary)
import qualified FDGEN.Template as Template

data CppBackend = CppBackend

instance Backend CppBackend
  where
  processDiscretised _ _ discreteForm = do
    template <- readFile "./templates/mesh.hpp.template"
    case Template.populate (buildTemplateDictionary discreteForm) template of
      Left err -> hPutStrLn stderr (show err) >> exitFailure
      Right generated -> putStrLn generated
