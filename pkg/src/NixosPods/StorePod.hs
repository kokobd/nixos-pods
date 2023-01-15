module NixosPods.StorePod
  ( lambda,
  )
where

import AWS.Lambda.Events.EventBridge (EventBridgeEvent)
import AWS.Lambda.Runtime (mRuntime)
import Effectful (runEff)
import Effectful.Resource (runResource)
import NixosPods.Infra.Amazonka (runAmazonka)
import NixosPods.Infra.Logger (runStdoutDefaultLogger)
import Relude

lambda :: IO ()
lambda =
  runEff
    . runResource
    . runStdoutDefaultLogger
    . runAmazonka
    . mRuntime
    $ \(_event :: EventBridgeEvent) -> pure "Hello, world."
