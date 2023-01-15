module NixosPods.Tool
  ( main,
  )
where

import Effectful.Resource (runResource)
import NixosPods.Infra.Amazonka (runAmazonka)
import NixosPods.Infra.Logger (runCmdLineAppLogger)
import NixosPods.Prelude
import NixosPods.Tool.Command (runCommandRunner, runCommand)
import NixosPods.Tool.Options (runOptionsE)
import NixosPods.Tool.Command.Deploy (runCommandDeploy)
import NixosPods.Infra.UUID (runUUIDGen)

main :: IO ()
main =
  -- The list of interpreters goes from the most basic, to more advanced.
  -- The later ones can depend on the former ones.
  runEff
    . runResource
    . runCmdLineAppLogger
    . runUUIDGen
    . runAmazonka
    . runOptionsE
    . runCommandDeploy
    . runCommandRunner
    $ runCommand
