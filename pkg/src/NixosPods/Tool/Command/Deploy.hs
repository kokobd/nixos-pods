{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module NixosPods.Tool.Command.Deploy
  ( CommandDeploy (..),
    run,
    runCommandDeploy,
  )
where

import Amazonka.CloudFormation
  ( ChangeSetStatus (..),
    ChangeSetType (..),
    newChangeSetCreateComplete,
    newCreateChangeSet,
    newDeleteChangeSet,
    newDescribeChangeSet,
    newDescribeStacks,
    newExecuteChangeSet,
  )
import Conduit qualified as C
import Control.Exception.Safe (throwIO)
import Control.Lens (view, (?~), (^.))
import Control.Monad.Logger
import Data.Aeson qualified as Json
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Dhall.JSON (CompileError, dhallToJSON, omitNull)
import NixosPods.Dhall qualified as Dhall
import NixosPods.Infra.Amazonka (Amazonka)
import NixosPods.Infra.Amazonka qualified as Amazonka
import NixosPods.Infra.Logger (Logger, runPureLoggingT)
import NixosPods.Prelude
import NixosPods.Tool.Options (OptionsE, getOptions)

data CommandDeploy :: Effect where
  Run :: CommandDeploy m ()

makeEffect ''CommandDeploy

newtype Error = DhallJSONError CompileError
  deriving stock (Show)

instance Exception Error

runCommandDeploy ::
  ( Amazonka :> es,
    Logger :> es,
    OptionsE :> es,
    IOE :> es -- UUID
  ) =>
  Eff (CommandDeploy : es) a ->
  Eff es a
runCommandDeploy = interpret $ \_ -> \case
  Run -> do
    -- deploy base.dhall
    baseTemplate <- case dhallToJSON Dhall.base of
      Left err -> throwIO $ DhallJSONError err
      Right x -> pure (omitNull x)
    stackNamePrefix <- view #stackNamePrefix <$> getOptions
    let stackName = stackNamePrefix <> "-base"
    existingStackConduit <- Amazonka.paginate newDescribeStacks
    responses <- C.runConduit $ existingStackConduit C..| C.sinkList
    let stacks = maybe [] concat $ traverse (view #stacks) responses
        stackExists = any (\stack -> stack ^. #stackName == stackName) stacks
        templateBody = decodeUtf8 . Json.encode $ baseTemplate
    changeSetName <- ("changeset-" <>) . UUID.toText <$> liftIO UUID.nextRandom
    void $
      Amazonka.send $
        newCreateChangeSet stackName changeSetName
          & #changeSetType
            ?~ (if stackExists then ChangeSetType_UPDATE else ChangeSetType_CREATE)
          & #templateBody
            ?~ templateBody
    void $
      Amazonka.await newChangeSetCreateComplete $
        newDescribeChangeSet changeSetName
          & #stackName ?~ stackName
    changeSet <- Amazonka.send $ newDescribeChangeSet changeSetName & #stackName ?~ stackName
    let status = changeSet ^. #status
        statusReason = changeSet ^. #statusReason
    if status == ChangeSetStatus_FAILED
      && maybe
        False
        ( "The submitted information didn't contain changes."
            `T.isInfixOf`
        )
        statusReason
      then do
        runPureLoggingT $ $(logInfo) "No change is required, skipping deployment"
        void $ Amazonka.send $ newDeleteChangeSet changeSetName & #stackName ?~ stackName
      else do
        runPureLoggingT $ $(logInfo) "Prepare to execute changeset"
        void $ Amazonka.send $ newExecuteChangeSet changeSetName & #stackName ?~ stackName

-- TODO deploy controller.dhall
