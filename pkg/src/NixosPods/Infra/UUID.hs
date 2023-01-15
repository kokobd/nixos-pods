module NixosPods.Infra.UUID
  ( UUIDGen (..),
    nextUUID,

    -- * Interpreters
    runUUIDGen,
  )
where

import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import NixosPods.Prelude

data UUIDGen :: Effect where
  NextUUID :: UUIDGen m UUID

makeEffect ''UUIDGen

runUUIDGen :: IOE :> es => Eff (UUIDGen : es) a -> Eff es a
runUUIDGen = interpret $ \_ -> \case
  NextUUID -> liftIO nextRandom
