module Capability.Navigate where

import Prelude

import Control.Monad.Trans.Class (lift)
import Halogen (HalogenM)

class Monad m <= Navigate m route | m -> route where
  navigate :: route -> m Unit

instance navigateHalogenM
         :: Navigate m route
         => Navigate (HalogenM state action slots output m) route
         where
  navigate = lift <<< navigate
