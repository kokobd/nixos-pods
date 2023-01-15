{-# LANGUAGE TemplateHaskell #-}

module NixosPods.Infra.Amazonka
  ( Amazonka (..),
    send,
    sendEither,
    paginate,
    paginateEither,
    await,
    awaitEither,
    runAmazonka,
  )
where

import Amazonka (AWSPager, AWSRequest, AWSResponse, Accept, Error, Wait)
import Amazonka qualified
import Data.Conduit (ConduitT, transPipe)
import Effectful (Eff, Effect, IOE, type (:>))
import Effectful.Dispatch.Dynamic (localSeqLift, reinterpret)
import Effectful.Reader.Dynamic (Reader, ask, runReader)
import Effectful.Resource (Resource)
import Effectful.TH (makeEffect)
import Relude hiding (Reader, ask, runReader)

data Amazonka :: Effect where
  Send :: AWSRequest a => a -> Amazonka m (AWSResponse a)
  SendEither :: AWSRequest a => a -> Amazonka m (Either Error (AWSResponse a))
  Paginate :: AWSPager a => a -> Amazonka m (ConduitT () (AWSResponse a) m ())
  PaginateEither :: AWSPager a => a -> Amazonka m (ConduitT () (AWSResponse a) m (Either Error ()))
  Await :: AWSRequest a => Wait a -> a -> Amazonka m Accept
  AwaitEither :: AWSRequest a => Wait a -> a -> Amazonka m (Either Error Accept)

makeEffect ''Amazonka

runAmazonka ::
  (Resource :> es, IOE :> es) =>
  Eff (Amazonka : es) a ->
  Eff es a
runAmazonka = reinterpret initAmazonka $ \localEnv -> \case
  Send req -> ask >>= flip Amazonka.send req
  SendEither req -> ask >>= flip Amazonka.sendEither req
  Paginate req -> do
    env <- ask
    localSeqLift localEnv $ \lift' ->
      pure $ transPipe lift' $ Amazonka.paginate env req
  PaginateEither req -> do
    env <- ask
    localSeqLift localEnv $ \lift' ->
      pure $ transPipe lift' $ Amazonka.paginateEither env req
  Await wait req -> do
    env <- ask
    Amazonka.await env wait req
  AwaitEither wait req -> do
    env <- ask
    Amazonka.awaitEither env wait req

initAmazonka ::
  IOE :> es =>
  Eff (Reader Amazonka.Env : es) a ->
  Eff es a
initAmazonka action = do
  env <- Amazonka.newEnv Amazonka.discover
  runReader env action
