{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Graphql where

import Import

import qualified Data.ByteString.Lazy.Char8 as B

import Data.Aeson (encode)
import Data.Morpheus (interpreter)
import Data.Morpheus.Document (importGQLDocumentWithNamespace)
import Data.Morpheus.Types as MT (GQLRootResolver (..), GQLRequest, GQLResponse, GQLType, IORes, Undefined(..))
import Yesod.Core.Types (JSONResponse(..))

newtype QueryG m = QueryG
  { products :: m ProductG
  } deriving (Generic, GQLType)

data ProductG = ProductG
  { name :: Text
  , price :: Int
  , quantity :: Int
  , source :: Maybe Text
  } deriving (Generic, GQLType)

rootResolver :: GQLRootResolver IO () QueryG Undefined Undefined
rootResolver =
  GQLRootResolver
    { queryResolver = QueryG {products = queryProducts}
    , mutationResolver = Undefined
    , subscriptionResolver = Undefined
    }
  where
    queryProducts = pure ProductG
      { name = "Vocal Modulator"
      , price = 125
      , quantity = 2
      , source = Just "http://aonsrd.com/Cybernetics.aspx?ItemName=Vocal%20Modulator&Family=None"
      }

graphqlApi :: GQLRequest -> IO GQLResponse
graphqlApi =
  interpreter rootResolver

postGraphqlR :: Handler (JSONResponse GQLResponse)
postGraphqlR = do
  body <- requireCheckJsonBody :: Handler GQLRequest
  result <- (liftIO . graphqlApi) body
  return $ JSONResponse result
