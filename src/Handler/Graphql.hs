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

import Import hiding (Query)

import qualified Data.ByteString.Lazy.Char8 as B

import Data.Aeson (withObject)
import Data.Morpheus (interpreter)
import Data.Morpheus.Document (importGQLDocumentWithNamespace)
import Data.Morpheus.Types as MT (GQLRootResolver (..), GQLRequest, GQLResponse, IORes, Undefined(..))

importGQLDocumentWithNamespace "config/schema.graphql"

rootResolver :: GQLRootResolver IO () Query Undefined Undefined
rootResolver =
  GQLRootResolver
    { queryResolver = Query {queryDeity}
    , mutationResolver = Undefined
    , subscriptionResolver = Undefined
    }
  where
    queryDeity QueryDeityArgs {queryDeityArgsName} = pure Deity
      { deityName = pure "Morpheus"
      , deityPower = pure (Just "Shapeshifting")
      }

graphqlApi :: GQLRequest -> IO GQLResponse
graphqlApi =
  interpreter rootResolver

postGraphqlR :: Handler Value
postGraphqlR = do
  body <- requireCheckJsonBody :: Handler GQLRequest
  result <- (liftIO . graphqlApi) body
  returnJson result