let Prelude =
      https://prelude.dhall-lang.org/v20.1.0/package.dhall
        sha256:26b0ef498663d269e4dc6a82b0ee289ec565d683ef4c00d0ebdd25333a5a3c98

let CFN =
      ./dhall-aws-cloudformation/package.dhall
        sha256:bf2cdcc3a4aa4804db580c415f86f0bb5a98ee803bb65a7a246480116f0e8873

let JSON = Prelude.JSON

let Fn = CFN.Fn

let DeletionPolicy = CFN.DeletionPolicy

let S3/Bucket = CFN.Cloudformation.`AWS::S3::Bucket`

let ECR/Repository = CFN.Cloudformation.`AWS::ECR::Repository`

-- meta resources needed to deploy the controller stack
-- we need some places to store the code
in  { Resources =
      { CodeBucket = S3/Bucket.Resources::{
        , DeletionPolicy = Some DeletionPolicy.Delete
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
      , DataCompressorLambdaECR = ECR/Repository.Resources::{
        , Properties = ECR/Repository.Properties::{
          , RepositoryPolicyText = Some
              ( JSON.object
                  ( toMap
                      { Version = JSON.string "2012-10-17"
                      , Statement =
                          JSON.array
                            [ JSON.object
                                ( toMap
                                    { Effect = JSON.string "Allow"
                                    , Principal =
                                        JSON.object
                                          ( toMap
                                              { Service =
                                                  JSON.string
                                                    "lambda.amazonaws.com"
                                              }
                                          )
                                    , Action =
                                        JSON.array
                                          [ JSON.string "ecr:BatchGetImage"
                                          , JSON.string
                                              "ecr:GetDownloadUrlForLayer"
                                          ]
                                    }
                                )
                            ]
                      }
                  )
              )
          }
        }
      }
    , Outputs =
      { CodeBucketName.Value = Fn.render (Fn.Ref "CodeBucket")
      , DataCompressorLambdaECRUri.Value
        =
          Fn.render
            (ECR/Repository.GetAttr.RepositoryUri "DataCompressorLambdaECR")
      }
    }
