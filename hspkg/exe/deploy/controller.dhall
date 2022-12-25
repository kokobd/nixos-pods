let Prelude = https://prelude.dhall-lang.org/v21.1.0/package.dhall

let JSON = Prelude.JSON

let Fn = ../../../deps/dhall-aws-cloudformation/Fn.dhall

let Lambda/Function =
      ../../../deps/dhall-aws-cloudformation/cloudformation/AWS::Lambda::Function.dhall

let ImageBuilder/Image =
      ../../../deps/dhall-aws-cloudformation/cloudformation/AWS::ImageBuilder::Image.dhall

in  { Resources =
      { CacheCompressorLambda = Lambda/Function.Resources::{
        , Properties = Lambda/Function.Properties::{
          , Code = Lambda/Function.Code::{
            , ImageUri = Some (Fn.renderText "image uri")
            }
          , Role = Fn.renderText "role"
          }
        }
      -- , CacheCompressorLambdaImage = ImageBuilder/Image.Resources::{
      --   , Properties = ImageBuilder/Image.Properties::{
      -- aws ecr create-repository --repository-name hello-world --image-scanning-configuration scanOnPush=true --image-tag-mutability MUTABLE
      --   }
      -- }
      , 
      }
    }
