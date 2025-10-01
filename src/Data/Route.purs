module Data.Route where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Routing.Duplex (RouteDuplex', optional, path, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Route
  = Logon
  | Logoff
  | Users (Maybe String)

derive instance genericRoute :: Generic Route _

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "Logon": path "logon" noArgs
  , "Logoff": path "logoff" noArgs
  , "Users": "users" / optional segment
  }
