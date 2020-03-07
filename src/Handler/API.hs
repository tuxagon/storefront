{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module Handler.API where

import Import

import Database.Persist.Sql
import Database.Persist.Sqlite
import Model

instance ToJSON Product where
    toJSON Product {..} = object
        [ "name" .= productName
        , "quantity" .= productQuantity
        , "price" .= productPrice
        , "source" .= productSource
        ]

getProductListR :: Handler Value
getProductListR =
  (return . array) [Product "Laser Pistol, Azimuth" 1 350 $ Just "http://aonsrd.com/WeaponDisplay.aspx?ItemName=Azimuth&Family=Laser%20Pistol"]

getProductDetailsR :: Key Product -> Handler Value
getProductDetailsR productId =
  returnJson $ Product "Laser Pistol, Azimuth" 1 350 $ Just "http://aonsrd.com/WeaponDisplay.aspx?ItemName=Azimuth&Family=Laser%20Pistol"

postNewProductR :: Handler Value
postNewProductR =
  return (String "postProductR")