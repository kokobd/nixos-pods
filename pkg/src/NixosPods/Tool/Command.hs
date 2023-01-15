module NixosPods.Tool.Command where

import Control.Lens ((^.))
import Data.Generics.Labels ()
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH
import NixosPods.Tool.Command.Deploy qualified as Deploy
import NixosPods.Tool.Options (Command (..), OptionsE, getOptions)
import Relude

data CommandRunner :: Effect where
  RunCommand :: CommandRunner m ()

makeEffect ''CommandRunner

runCommandRunner ::
  ( OptionsE :> es,
    Deploy.CommandDeploy :> es
  ) =>
  Eff (CommandRunner : es) a ->
  Eff es a
runCommandRunner = interpret $ \_ -> \case
  RunCommand -> do
    opts <- getOptions
    case opts ^. #command of
      CommandDeploy -> Deploy.run
