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

import Amazonka (Accept (AcceptSuccess), Wait (..), name, wait_acceptors)
import Amazonka.CloudFormation
  ( Capability (Capability'),
    ChangeSetStatus (..),
    ChangeSetType (..),
    Output,
    newChangeSetCreateComplete,
    newCreateChangeSet,
    newDeleteChangeSet,
    newDescribeChangeSet,
    newDescribeStacks,
    newExecuteChangeSet,
    newParameter,
    newStackCreateComplete,
    newStackUpdateComplete,
  )
import Conduit qualified as C
import Control.Exception.Safe (MonadThrow, throw)
import Control.Lens
import Control.Monad.Logger
import Data.Map.Strict qualified as Map
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Dhall.JSON (CompileError)
import NixosPods.Dhall qualified as Dhall
import NixosPods.Infra.Amazonka (Amazonka)
import NixosPods.Infra.Amazonka qualified as Amazonka
import NixosPods.Infra.Amazonka.ECRHelper (ECRHelper, uploadImages)
import NixosPods.Infra.Logger (Logger, runPureLoggingT)
import NixosPods.Infra.UUID (UUIDGen)
import NixosPods.Infra.UUID qualified as UUID
import NixosPods.Prelude
import NixosPods.Tool.Environment (EnvVarsE, askEnvVar)
import NixosPods.Tool.Options (OptionsE, getOptions)
import System.FilePath ((</>))

data CommandDeploy :: Effect where
  Run :: CommandDeploy m ()

makeEffect ''CommandDeploy

data Error
  = DhallJSONError CompileError
  | CfnStackUpdateFailure {stackName :: Text}
  | StackOutputNotFoundError {stackName :: Text, outputKey :: Maybe Text}
  | NoExecutableForECR {ecrLogicalName :: Text}
  deriving stock (Show)

instance Exception Error where
  displayException (DhallJSONError err) = [i|Failed to convert dhall to JSON: #{err}|]
  displayException (CfnStackUpdateFailure {..}) = [i|Failed to update Cloudformation stack #{stackName}|]
  displayException (StackOutputNotFoundError {..}) =
    maybe
      [i|Failed to find any output of stack #{stackName}|]
      (\key -> [i|Output #{key} not found in stack #{stackName}|])
      outputKey
  displayException NoExecutableForECR {..} = [i|No exeuctable for ECR #{ecrLogicalName}|]

runCommandDeploy ::
  ( Amazonka :> es,
    Logger :> es,
    OptionsE :> es,
    UUIDGen :> es,
    EnvVarsE :> es,
    ECRHelper :> es
  ) =>
  Eff (CommandDeploy : es) a ->
  Eff es a
runCommandDeploy = interpret $ \_ -> \case
  Run -> do
    stackNamePrefix <- view #stackNamePrefix <$> getOptions
    let makeStackName :: Text -> Text
        makeStackName name = stackNamePrefix <> "-" <> name
    baseStack <- deployBaseStack makeStackName
    uploadedDockerImages <- uploadServiceImages baseStack
    deployControllerStack makeStackName uploadedDockerImages

data BaseStack = BaseStack
  { s3BucketName :: Text,
    -- | from service executable name, to URI of the ECR repository
    ecrUris :: Map Text Text
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
  let stackName = makeStackName "base"
  deployStack stackName Dhall.baseStack mempty
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
parseBaseStack stackName outputs = do
  s3BucketName <- expectField "GeneralBucketName"
  pure BaseStack {s3BucketName, ecrUris}
  where
    outputsMap :: Map Text Text
    outputsMap = fromList $ mapMaybe (\o -> liftA2 (,) (o ^. #outputKey) (o ^. #outputValue)) outputs

    expectField :: Text -> m Text
    expectField key =
      maybe (throw StackOutputNotFoundError {stackName, outputKey = Just key}) pure (outputsMap Map.!? key)

    ecrUris =
      fromList
        . mapMaybe (\(k, v) -> (,v) <$> T.stripPrefix "ECR" k)
        . Map.toList
        $ outputsMap

-- | from image logical name (without "ECR-" prefix) to image uri
newtype UploadedDockerImages = UploadedDockerImages (Map Text Text)

uploadServiceImages ::
  ( EnvVarsE :> es,
    Logger :> es,
    Amazonka :> es,
    ECRHelper :> es
  ) =>
  BaseStack ->
  Eff es UploadedDockerImages
uploadServiceImages BaseStack {ecrUris} = do
  imagesDir <- askEnvVar (view #serviceImagesDir)
  runPureLoggingT $ do
    $(logDebug) [i|imagesDir: #{imagesDir}|]
    $(logDebug) [i|ecrUris: #{ecrUris}|]
  localImagesWithRepoUris <- fmap Map.elems $ flip Map.traverseWithKey ecrUris $ \name uri -> do
    filename <-
      maybe (throw NoExecutableForECR {ecrLogicalName = name}) pure $
        Dhall.services Map.!? name
    let imagePath = imagesDir </> T.unpack filename
    pure (name, (imagePath, uri))
  imageUris <- uploadImages (fmap snd localImagesWithRepoUris)
  pure . UploadedDockerImages . Map.fromList $ zip (fmap fst localImagesWithRepoUris) imageUris

deployControllerStack ::
  ( Amazonka :> es,
    Logger :> es,
    UUIDGen :> es
  ) =>
  (Text -> Text) ->
  UploadedDockerImages ->
  Eff es ()
deployControllerStack makeStackName (UploadedDockerImages ecrLogicalNameToImageUri) = do
  deployStack stackName Dhall.controllerStack parameters
  pure ()
  where
    stackName = makeStackName "controller"
    parameters = Map.fromList . fmap (first ("ImageUri" <>)) . Map.toList $ ecrLogicalNameToImageUri

deployStack ::
  ( Amazonka :> es,
    Logger :> es,
    UUIDGen :> es
  ) =>
  -- | stack name
  Text ->
  -- | template body
  Text ->
  -- | template parameter values
  Map Text Text ->
  Eff es ()
deployStack stackName templateBody parameters = do
  runPureLoggingT $ $(logInfo) [i|start deploying #{stackName}|]
  existingStackConduit <- Amazonka.paginate newDescribeStacks
  responses <- C.runConduit $ existingStackConduit C..| C.sinkList
  let stacks = maybe [] concat $ traverse (view #stacks) responses
      stackExists = any (\stack -> stack ^. #stackName == stackName) stacks
  changeSetName <- ("changeset-" <>) . UUID.toText <$> UUID.nextUUID
  void $
    Amazonka.send $
      newCreateChangeSet stackName changeSetName
        & #changeSetType
          ?~ (if stackExists then ChangeSetType_UPDATE else ChangeSetType_CREATE)
        & #templateBody
          ?~ templateBody
        & #parameters
          ?~ ( fmap
                 ( \(name, value) ->
                     newParameter
                       & #parameterKey ?~ name
                       & #parameterValue ?~ value
                 )
                 . Map.toList
             )
            parameters
        & #capabilities
          ?~ [Capability' "CAPABILITY_IAM"]
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
      runPureLoggingT $ $(logInfo) "No change is required, skipping deployment of"
      void $ Amazonka.send $ newDeleteChangeSet changeSetName & #stackName ?~ stackName
    else do
      runPureLoggingT $ $(logInfo) "Start executing changeset"
      void $ Amazonka.send $ newExecuteChangeSet changeSetName & #stackName ?~ stackName
  accept <-
    Amazonka.await
      ( Wait
          { name = "StackCreateOrUpdateComplete",
            attempts = 1200,
            delay = 3,
            Amazonka.acceptors =
              (newStackCreateComplete ^. wait_acceptors)
                <> (newStackUpdateComplete ^. wait_acceptors)
          }
      )
      $ newDescribeStacks & #stackName ?~ stackName
  when (accept /= AcceptSuccess) $ do
    runPureLoggingT $ $(logError) "Failed to deploy base stack, please check it in AWS console"
    throw CfnStackUpdateFailure {stackName}
  runPureLoggingT $ $(logInfo) [i|Stack #{stackName} is ready|]
