{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE MultiWayIf #-}

module NixosPods.Infra.Logger
  ( Logger(..)
  , PureLoggingT
  , runPureLoggingT
  -- * Interpreters
  , runStdoutDefaultLogger
  , runCmdLineAppLogger
  ) where

import Control.Lens (view, (^.))
import Control.Monad.Logger
  ( Loc,
    LogLevel (..),
    LogSource,
    LogStr,
    MonadLogger (..),
    MonadLoggerIO (..),
    ToLogStr (..),
    defaultOutput,
  )
import Data.ByteString qualified as BS
import Data.Generics.Labels ()
import System.Log.FastLogger (fromLogStr)
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
import Effectful.Reader.Static (ask, runReader)
import Effectful.TH (makeEffect)
import Relude hiding (Reader, ask, runReader)

data Logger :: Effect where
  RunPureLoggingT :: PureLoggingT m a -> Logger m a

type LogFunc m = Loc -> LogSource -> LogLevel -> LogStr -> m ()

newtype PureLoggingT m a = PureLoggingT
  { runPureLoggingT' :: ReaderT (LogFunc m) m a
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
  withRunInIO action = PureLoggingT $ withRunInIO $ \runInIO -> action $ runInIO . view #runPureLoggingT'

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
  Eff es (LogFunc IO) ->
  Eff (Logger : es) a ->
  Eff es a
runLogger initLogFunc = reinterpret (\action -> initLogFunc >>= flip runReader action) $ \env -> \case
  RunPureLoggingT action -> do
    logFunc :: LogFunc IO <- ask
    localLiftUnliftIO env SeqUnlift $ \liftEff unlift -> do
      unlift $ runReaderT (action ^. #runPureLoggingT') $ \loc source level msg -> liftEff $ logFunc loc source level msg

runStdoutDefaultLogger :: IOE :> es => Eff (Logger : es) a -> Eff es a
runStdoutDefaultLogger = runLogger $ pure (defaultOutput stdout)

runCmdLineAppLogger :: IOE :> es => Eff (Logger : es) a -> Eff es a
runCmdLineAppLogger = runLogger $
  pure $ \loc source level msg ->
    if | level == LevelDebug -> defaultOutput stdout loc source level msg
       | level <= LevelWarn -> BS.hPut stdout $ fromLogStr msg <> "\n"
       | otherwise -> BS.hPut stderr $ fromLogStr msg <> "\n"
