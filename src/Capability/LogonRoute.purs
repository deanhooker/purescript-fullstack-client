module Capability.LogonRoute where

import Prelude

import Control.Monad.Trans.Class (lift)
import Halogen (HalogenM)

data PasswordType = PasswordPermanent | PasswordTemporary

class Monad m <= LogonRoute m route | m -> route where
  logonRoute :: PasswordType -> m route

instance logonRouteHalogenM
         :: LogonRoute m route
         => LogonRoute (HalogenM state action slots output m) route where
  logonRoute = lift <<< logonRoute
