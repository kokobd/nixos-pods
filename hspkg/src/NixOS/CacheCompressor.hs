module NixOS.CacheCompressor
  ( lambda
  ) where

import AWS.Lambda.Runtime
import AWS.Lambda.Events.EventBridge
import Relude

lambda :: IO ()
lambda = pureRuntime $ \(_event :: EventBridgeEvent) -> "Hello, world."
