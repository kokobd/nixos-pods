let Prelude =
      https://prelude.dhall-lang.org/v20.1.0/package.dhall
        sha256:26b0ef498663d269e4dc6a82b0ee289ec565d683ef4c00d0ebdd25333a5a3c98

let JSON = Prelude.JSON

let CFN =
      ./dhall-aws-cloudformation/package.dhall
        sha256:bf2cdcc3a4aa4804db580c415f86f0bb5a98ee803bb65a7a246480116f0e8873

let Fn = CFN.Fn

let Lambda/Function = CFN.Cloudformation.`AWS::Lambda::Function`

let IAM/Role = CFN.Cloudformation.`AWS::IAM::Role`

in  { Parameters.DataCompressorLambdaImageUri
      =
      { Type = "String", Description = "image uri for data-compressor-lambda" }
    , Resources =
      { DataCompressorFunction = Lambda/Function.Resources::{
        , Properties = Lambda/Function.Properties::{
          , Code = Lambda/Function.Code::{
            , ImageUri = Some
                (Fn.render (Fn.Ref "DataCompressorLambdaImageUri"))
            }
          , PackageType = Some (Fn.renderText "Image")
          , MemorySize = Some +128
          , Role = Fn.render (Fn.Ref "DataCompressorFunctionRole")
          }
        }
      , DataCompressorFunctionRole = IAM/Role.Resources::{
        , Properties = IAM/Role.Properties::{
          , AssumeRolePolicyDocument =
              JSON.object
                [ { mapKey = "Version", mapValue = JSON.string "2012-10-17" }
                , { mapKey = "Statement"
                  , mapValue =
                      JSON.array
                        [ JSON.object
                            [ { mapKey = "Effect"
                              , mapValue = JSON.string "Allow"
                              }
                            , { mapKey = "Principal"
                              , mapValue =
                                  JSON.object
                                    [ { mapKey = "Service"
                                      , mapValue =
                                          JSON.string "ec2.amazonaws.com"
                                      }
                                    ]
                              }
                            , { mapKey = "Action"
                              , mapValue = JSON.string "sts:AssumeRole"
                              }
                            ]
                        ]
                  }
                ]
          }
        }
      }
    }
