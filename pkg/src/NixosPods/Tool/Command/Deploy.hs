{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module NixosPods.Tool.Command.Deploy
  ( CommandDeploy (..),
    run,
    runCommandDeploy,
  )
where

import Amazonka (Accept (AcceptSuccess))
import Amazonka.CloudFormation
  ( ChangeSetStatus (..),
    ChangeSetType (..),
    Output,
    newChangeSetCreateComplete,
    newCreateChangeSet,
    newDeleteChangeSet,
    newDescribeChangeSet,
    newDescribeStacks,
    newExecuteChangeSet,
    newStackUpdateComplete,
  )
import Conduit qualified as C
import Control.Exception.Safe (MonadThrow, throw)
import Control.Lens (view, (?~), (^.))
import Control.Monad.Logger
import Data.Aeson qualified as Json
import Data.Map.Strict qualified as Map
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Dhall.JSON (CompileError, dhallToJSON, omitNull)
import NixosPods.Dhall qualified as Dhall
import NixosPods.Infra.Amazonka (Amazonka)
import NixosPods.Infra.Amazonka qualified as Amazonka
import NixosPods.Infra.Logger (Logger, runPureLoggingT)
import NixosPods.Infra.UUID (UUIDGen)
import NixosPods.Infra.UUID qualified as UUID
import NixosPods.Prelude
import NixosPods.Tool.Options (OptionsE, getOptions)

data CommandDeploy :: Effect where
  Run :: CommandDeploy m ()

makeEffect ''CommandDeploy

data Error
  = DhallJSONError CompileError
  | CfnStackUpdateFailure {stackName :: Text}
  | StackOutputNotFoundError {stackName :: Text, outputKey :: Maybe Text}
  deriving stock (Show)

instance Exception Error where
  displayException (DhallJSONError err) = [i|Failed to convert dhall to JSON: #{err}|]
  displayException (CfnStackUpdateFailure {..}) = [i|Failed to update Cloudformation stack #{stackName}|]
  displayException (StackOutputNotFoundError {..}) =
    maybe
      [i|Failed to find any output of stack #{stackName}|]
      (\key -> [i|Output #{key} not found in stack #{stackName}|])
      outputKey

runCommandDeploy ::
  ( Amazonka :> es,
    Logger :> es,
    OptionsE :> es,
    UUIDGen :> es
  ) =>
  Eff (CommandDeploy : es) a ->
  Eff es a
runCommandDeploy = interpret $ \_ -> \case
  Run -> do
    stackNamePrefix <- view #stackNamePrefix <$> getOptions
    let makeStackName :: Text -> Text
        makeStackName name = stackNamePrefix <> "-" <> name
    baseStack <- deployBaseStack makeStackName
    pure ()

-- TODO deploy controller.dhall
-- 1. upload docker images
-- 2. deploy controller.dhall using command line tool skopeo

newtype BaseStack = BaseStack
  { dataCompressorLambdaECRUri :: Text
  }
  deriving stock (Show, Eq, Generic)

deployBaseStack ::
  ( Amazonka :> es,
    Logger :> es,
    UUIDGen :> es
  ) =>
  (Text -> Text) ->
  Eff es BaseStack
deployBaseStack makeStackName = do
  baseTemplate <- case dhallToJSON Dhall.base of
    Left err -> throw $ DhallJSONError err
    Right x -> pure (omitNull x)
  let stackName = makeStackName "base"
  existingStackConduit <- Amazonka.paginate newDescribeStacks
  responses <- C.runConduit $ existingStackConduit C..| C.sinkList
  let stacks = maybe [] concat $ traverse (view #stacks) responses
      stackExists = any (\stack -> stack ^. #stackName == stackName) stacks
      templateBody = decodeUtf8 . Json.encode $ baseTemplate
  changeSetName <- ("changeset-" <>) . UUID.toText <$> UUID.nextUUID
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
  accept <- Amazonka.await newStackUpdateComplete $ newDescribeStacks & #stackName ?~ stackName
  when (accept /= AcceptSuccess) $ do
    runPureLoggingT $ $(logError) "Failed to deploy controller stack, please check it in AWS console"
    throw CfnStackUpdateFailure {stackName}
  getBaseStackOutput stackName

getBaseStackOutput ::
  ( Amazonka :> es,
    Logger :> es
  ) =>
  Text ->
  Eff es BaseStack
getBaseStackOutput stackName = do
  stacks <-
    fmap (view #stacks)
      . Amazonka.send
      $ newDescribeStacks & #stackName ?~ stackName
  case stacks of
    Just [stack] -> do
      outputs <-
        maybe
          (throw StackOutputNotFoundError {stackName, outputKey = Nothing})
          pure
          (stack ^. #outputs)
      parseBaseStack stackName outputs
    _ -> do
      runPureLoggingT $ $(logError) "Can't find the stack we just created. Maybe it's deleted manually"
      throw CfnStackUpdateFailure {stackName}

parseBaseStack :: forall m. MonadThrow m => Text -> [Output] -> m BaseStack
parseBaseStack stackName outputs =
  BaseStack
    <$> expectField "DataCompressorLambdaECRUri"
  where
    outputsMap :: Map Text Text
    outputsMap = fromList $ mapMaybe (\o -> liftA2 (,) (o ^. #outputKey) (o ^. #outputValue)) outputs

    expectField :: Text -> m Text
    expectField key =
      maybe (throw StackOutputNotFoundError {stackName, outputKey = Just key}) pure (outputsMap Map.!? key)
