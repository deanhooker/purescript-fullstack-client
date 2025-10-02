module AppM where

import Prelude

import Capability.Log (class Log)
import Capability.LogonRoute (class LogonRoute, PasswordType(..))
import Capability.Navigate (class Navigate)
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Route (Route)
import Data.Route as Route
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Effect.Now (nowDateTime)
import Env (Env)
import Halogen (liftEffect)
import Routing.Duplex as RouteDuplex
import Routing.Hash (setHash)

newtype AppM a = AppM (ReaderT Env Aff a)
derive instance newtypeAppM :: Newtype (AppM a) _
derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
derive newtype instance monadAskAppM :: MonadAsk Env AppM

instance logonRouteAppM :: LogonRoute AppM Route where
  logonRoute PasswordTemporary = pure Route.ChangePassword
  logonRoute PasswordPermanent = pure $ Route.Users Nothing

instance navigateAppM :: Navigate AppM Route where
  navigate = liftEffect <<< setHash <<< RouteDuplex.print Route.routeCodec

instance logAppM :: Log AppM where
  logEntry level message = liftEffect nowDateTime >>= pure <<< { level, message, timestamp: _}
  log entry = log $ show entry

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM reader) = runReaderT reader env
