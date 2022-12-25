{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module NixOS.Infra.Logger where

import Control.Lens (view, (^.))
import Control.Monad.Logger
  ( Loc,
    LogLevel,
    LogSource,
    LogStr,
    MonadLogger (..),
    MonadLoggerIO (..),
    ToLogStr (..),
    defaultOutput,
  )
import Data.Generics.Labels ()
import Effectful
  ( Eff,
    Effect,
    IOE,
    Limit (Unlimited),
    MonadUnliftIO (..),
    Persistence (Ephemeral),
    UnliftStrategy (ConcUnlift, SeqUnlift),
    withEffToIO,
    withUnliftStrategy,
    type (:>),
  )
import Effectful.Dispatch.Dynamic
  ( localLiftUnliftIO,
    reinterpret,
  )
import Effectful.Reader.Static (Reader, ask, runReader)
import Effectful.TH (makeEffect)
import Relude hiding (Reader, ask, runReader)

data Logger :: Effect where
  WithLogger :: PureLoggingT m a -> Logger m a

type LogFunc m = Loc -> LogSource -> LogLevel -> LogStr -> m ()

newtype PureLoggingT m a = PureLoggingT
  { runPureLoggingT :: ReaderT (LogFunc m) m a
  }
  deriving newtype (Functor, Applicative, Monad)
  deriving stock (Generic)

instance Monad m => MonadLogger (PureLoggingT m) where
  monadLoggerLog :: ToLogStr msg => Loc -> LogSource -> LogLevel -> msg -> PureLoggingT m ()
  monadLoggerLog loc source level msg = PureLoggingT $ ReaderT $ \logFunc ->
    logFunc loc source level (toLogStr msg)

instance MonadIO m => MonadIO (PureLoggingT m) where
  liftIO :: IO a -> PureLoggingT m a
  liftIO = PureLoggingT . liftIO

instance MonadUnliftIO m => MonadUnliftIO (PureLoggingT m) where
  withRunInIO :: ((forall a. PureLoggingT m a -> IO a) -> IO b) -> PureLoggingT m b
  withRunInIO action = PureLoggingT $ withRunInIO $ \runInIO -> action $ runInIO . view #runPureLoggingT

instance IOE :> es => MonadLoggerIO (PureLoggingT (Eff es)) where
  askLoggerIO :: PureLoggingT (Eff es) (LogFunc IO)
  askLoggerIO = PureLoggingT $ ReaderT $ \logFunc -> do
    withUnliftStrategy (ConcUnlift Ephemeral Unlimited) $ withEffToIO $ \unlift ->
      pure $ \loc source level msg -> unlift (logFunc loc source level msg)

instance MonadTrans PureLoggingT where
  lift :: Monad m => m a -> PureLoggingT m a
  lift = PureLoggingT . lift

makeEffect ''Logger

runLogger ::
  IOE :> es =>
  Eff (Logger : es) a ->
  Eff es a
runLogger = reinterpret initLogger $ \env -> \case
  WithLogger action -> do
    logFunc :: LogFunc IO <- ask
    localLiftUnliftIO env SeqUnlift $ \liftEff unlift -> do
      unlift $ runReaderT (action ^. #runPureLoggingT) $ \loc source level msg -> liftEff $ logFunc loc source level msg

initLogger :: Eff (Reader (LogFunc IO) : es) a -> Eff es a
initLogger action = do
  let logFunc = defaultOutput stdout
  runReader logFunc action
