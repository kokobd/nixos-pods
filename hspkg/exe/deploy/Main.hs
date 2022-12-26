{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Generics.Labels ()
import Dhall.JSON (dhallToJSON)
import Options (Command (..), Options (..), parseOptions)
import Relude
import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID
import Embed
import Amazonka.CloudFormation
import qualified Data.Text.IO as T

main :: IO ()
main = do
  options <- parseOptions
  case options ^. #command of
    Base -> handleError $ deployBase options
    Controller -> undefined

  pure ()

handleError :: ExceptT Text IO () -> IO ()
handleError exceptT = runExceptT exceptT >>= \case
  Right _ -> pure ()
  Left errMsg -> do
    T.putStrLn $ "error: " <> errMsg
    exitFailure

deployBase :: Options -> ExceptT Text IO ()
deployBase options = do
  baseStackJson <- ExceptT . pure . first show $ dhallToJSON baseDhall
  changeSetName <- UUID.toText <$> liftIO UUID.nextRandom
  let x = newCreateChangeSet stackName changeSetName & #changeSetType ?~ ChangeSetType_CREATE

  pure ()
    -- baseDhall = $(makeRelativeToProject "exe/deploy/base.dhall" >>= staticDhallExpression . T.pack)
  where
    stackName = options ^. #stackNamePrefix <> "-base"

{-
deployBase :: NixBuilder ()
deployBase = undefined
-- 1. dhall to yaml
-- 2. create changeset
-- 3. apply changeset

deployController :: NixBuilder ()
deployController = do
  deployBase
-- 1. pack all executables
  -- TODO
-}