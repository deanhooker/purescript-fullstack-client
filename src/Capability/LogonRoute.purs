module Capability.LogonRoute where

import Data.UUID (UUID)

class LogonRoute route where
  logonRoute :: UUID -> PasswordType -> route

data PasswordType = PasswordPermanent | PasswordTemporary
