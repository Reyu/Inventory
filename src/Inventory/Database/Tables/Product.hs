{- |
Module: Inventory.Database.Tables.Product
Description: Products and Manufacturers
Copyright: (c) 2021 Tim Millican
SPDX-License-Identifier: MIT
Maintainer: Tim Millican <reyu@reyuzenfold.com>
Stability: experimental

-}

module Inventory.Database.Tables.Product
  ( -- * Primary Keys
    PrimaryKey(ProductId)
    -- * Table
  , ProductT
    -- * Simple Table Structure
  , Product
    -- ** Lenses
  , productId
  , productName
  , productDescription
  , productManufacturer
  ) where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                , genericParseJSON
                                                , genericToJSON
                                                , parseJSON
                                                , toJSON
                                                )
import           Database.Beam                  ( Beamable
                                                , Columnar
                                                , LensFor(LensFor)
                                                , Table(..)
                                                , tableLenses
                                                )
import           Database.Beam.Backend          ( SqlSerial )
import           Inventory.Database.Utils       ( defaultJSONOptions )

data ProductT f = Product
    { _productId :: Columnar f (SqlSerial Int32)
    , _productName :: Columnar f Text
    , _productDescription :: Columnar f Text
    , _productManufacturer :: Columnar f Int32
    }
    deriving stock Generic
    deriving anyclass Beamable
instance Table ProductT where
    data PrimaryKey ProductT f = ProductId (Columnar f (SqlSerial Int32))
        deriving stock Generic
        deriving anyclass Beamable
    primaryKey = ProductId <$> _productId

type Product = ProductT Identity
deriving stock instance Show Product
deriving stock instance Eq Product
instance FromJSON Product where
    parseJSON = genericParseJSON defaultJSONOptions
instance ToJSON Product where
    toJSON = genericToJSON defaultJSONOptions

Product (LensFor productId) (LensFor productName) (LensFor productDescription) (LensFor productManufacturer) = tableLenses
