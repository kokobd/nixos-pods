module NixosPods.Tool
  ( main,
  )
where

import Effectful.Resource (runResource)
import NixosPods.Infra.Amazonka (runAmazonka)
import NixosPods.Infra.Amazonka.ECRHelper (runECRHelper)
import NixosPods.Infra.Clock (runClock)
import NixosPods.Infra.Logger (runCmdLineAppLogger)
import NixosPods.Infra.UUID (runUUIDGen)
import NixosPods.Prelude
import NixosPods.Tool.Command (runCommand, runCommandRunner)
import NixosPods.Tool.Command.Deploy (runCommandDeploy)
import NixosPods.Tool.Environment (runEnvVarsE)
import NixosPods.Tool.Options (runOptionsE)

main :: IO ()
main =
  -- The list of interpreters goes from the most basic, to more advanced.
  -- The later ones can depend on the former ones.
  runEff
    . runResource
    . runCmdLineAppLogger
    . runEnvVarsE
    . runClock
    . runUUIDGen
    . runAmazonka
    . runECRHelper
    . runOptionsE
    . runCommandDeploy
    . runCommandRunner
    $ runCommand
