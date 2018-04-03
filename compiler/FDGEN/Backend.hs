module FDGEN.Backend (Backend(..), Options(..), defaultOptions) where
import FDGEN.Discrete (Discretised)

data Options = Options
  { _optionsInputPath :: String
  }

defaultOptions :: Options
defaultOptions = Options
  { _optionsInputPath = error "No input path available"
  }

class Backend e where
  processDiscretised :: e -> Options -> Discretised -> IO()

