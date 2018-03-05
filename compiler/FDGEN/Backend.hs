module FDGEN.Backend (Backend(..)) where
import FDGEN.Discrete (Discretised)

class Backend e where
  processDiscretised :: e -> Discretised -> IO()

