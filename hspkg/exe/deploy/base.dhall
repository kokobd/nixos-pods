let Prelude = https://prelude.dhall-lang.org/v21.1.0/package.dhall

let JSON = Prelude.JSON

let Fn = ../../../deps/dhall-aws-cloudformation/Fn.dhall

let DeletionPolicy = ../../../deps/dhall-aws-cloudformation/DeletionPolicy.dhall

let S3/Bucket =
      ../../../deps/dhall-aws-cloudformation/cloudformation/AWS::S3::Bucket.dhall

let ECR/Repository =
      ../../../deps/dhall-aws-cloudformation/cloudformation/AWS::ECR::Repository.dhall

in  { Resources =
      { GeneralBucket = S3/Bucket.Resources::{
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
      , ContainerRepository = ECR/Repository.Resources::{
        , Properties = ECR/Repository.Properties::{
          , RepositoryPolicyText = Some (JSON.string "abc")
          }
        }
      }
    , Outputs =
      { GeneralBucketName.Value = Fn.render (Fn.Ref "GeneralBucket")
      , ContainerRepositoryUri.Value
        = Fn.render (ECR/Repository.GetAttr.RepositoryUri "ContainerRepository")
      }
    }
