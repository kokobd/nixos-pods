{-# LANGUAGE TemplateHaskell #-}

module NixOS.Infra.Amazonka
  (
  )
where

import Amazonka (AWSRequest, AWSResponse, Error)
import Effectful (Effect)
import Effectful.TH (makeEffect)
import Relude
import Data.Conduit

data Amazonka :: Effect where
  Send :: AWSRequest a => a -> Amazonka m (AWSResponse a)
  SendEither :: AWSRequest a => a -> Amazonka m (Either Error (AWSResponse a))
  Paginate :: AWSRequest a => a -> ConduitT () (AWSResponse a) m ()

makeEffect ''Amazonka
