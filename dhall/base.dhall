let Prelude =
      https://prelude.dhall-lang.org/v20.1.0/package.dhall
        sha256:26b0ef498663d269e4dc6a82b0ee289ec565d683ef4c00d0ebdd25333a5a3c98

let CloudFormation =
      ./dhall-aws-cloudformation/package.dhall
        sha256:bf2cdcc3a4aa4804db580c415f86f0bb5a98ee803bb65a7a246480116f0e8873

let JSON = Prelude.JSON

let Fn = CloudFormation.Fn

let DeletionPolicy = CloudFormation.DeletionPolicy

let S3/Bucket = CloudFormation.Cloudformation.`AWS::S3::Bucket`

in  { Resources.GeneralBucket
      = S3/Bucket.Resources::{
      , DeletionPolicy = Some DeletionPolicy.Retain
      , Properties = S3/Bucket.Properties::{
        , LifecycleConfiguration = Some S3/Bucket.LifecycleConfiguration::{
          , Rules =
            [ S3/Bucket.Rule::{
              , Transitions = Some
                [ S3/Bucket.Transition::{
                  , StorageClass = Fn.renderText "INTELLIGENT_TIERING"
                  , TransitionInDays = Some +1
                  }
                ]
              , Status = Fn.renderText "Enabled"
              }
            ]
          }
        }
      }
    , Outputs.GeneralBucketName.Value = Fn.render (Fn.Ref "GeneralBucket")
    }
