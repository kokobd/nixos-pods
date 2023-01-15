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

main :: IO ()
main =
  runEff
    . runResource
    . runCmdLineAppLogger
    . runAmazonka
    . runOptionsE
    . runCommandDeploy
    . runCommandRunner
    $ runCommand
