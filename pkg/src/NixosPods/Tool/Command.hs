{-# LANGUAGE QuasiQuotes #-}

module NixosPods.Tool.Command where

import Control.Lens ((^.))
import Data.Generics.Labels ()
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH
import NixosPods.Tool.Command.Deploy qualified as Deploy
import NixosPods.Tool.Options (Command (..), OptionsE, getOptions)
import Relude
import Control.Exception.Safe (catchAny)
import NixosPods.Infra.Logger (Logger, runPureLoggingT)
import Data.String.Interpolate (i)
import Control.Monad.Logger (logError)

data CommandRunner :: Effect where
  RunCommand :: CommandRunner m ()

makeEffect ''CommandRunner

runCommandRunner ::
  ( OptionsE :> es,
    Logger :> es,
    Deploy.CommandDeploy :> es
  ) =>
  Eff (CommandRunner : es) a ->
  Eff es a
runCommandRunner = interpret $ \_ -> \case
  RunCommand ->
    flip catchAny handleException $ do
      opts <- getOptions
      case opts ^. #command of
        CommandDeploy -> Deploy.run

handleException :: 
  Logger :> es => 
  SomeException -> Eff es ()
handleException e =
  runPureLoggingT $ $(logError) [i|Execution aborted due to error: #{displayException e}|]
