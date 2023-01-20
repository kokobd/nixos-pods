module NixosPods.Infra.Clock
  ( Clock (..),
    getCurrentTime,
    unixTimestampNano,

    -- * Interpreters
    runClock,
  )
where

import Data.Time (UTCTime)
import Data.Time qualified as Time
import Data.Time.Clock.POSIX qualified as Time
import NixosPods.Prelude

data Clock :: Effect where
  GetCurrentTime :: Clock m UTCTime

makeEffect ''Clock

unixTimestampNano :: UTCTime -> Int64
unixTimestampNano =
  floor . (1e9 *) . Time.nominalDiffTimeToSeconds . Time.utcTimeToPOSIXSeconds

runClock ::
  IOE :> es =>
  Eff (Clock : es) a ->
  Eff es a
runClock = interpret $ \_ -> \case
  GetCurrentTime -> liftIO Time.getCurrentTime
