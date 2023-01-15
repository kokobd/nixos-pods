module NixosPods.Prelude
  ( module Relude,
    module Effectful,
    makeEffect,
    interpret,
    reinterpret,
  )
where

import Effectful
import Effectful.Dispatch.Dynamic (interpret, reinterpret)
import Effectful.TH (makeEffect)
import Relude hiding (Reader, ask, asks, runReader)
