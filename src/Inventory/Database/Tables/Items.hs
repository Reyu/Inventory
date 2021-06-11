{- |
Copyright: (c) 2021 Reyu Zenfold
SPDX-License-Identifier: MIT
Maintainer: Reyu Zenfold <reyu@reyuzenfold.com>

-}

-- This is disabled for the Lens definitions
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Inventory.Database.Tables.Items
  ( ItemT(Item)
  , ItemID
  , itemId
  , itemCount
  , itemLocation
  , itemUnit
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
import           Inventory.Database.Tables.Locations
                                                ( LocationT
                                                , PrimaryKey(LocationId)
                                                )
import           Inventory.Database.Tables.Units
                                                ( PrimaryKey(UnitId)
                                                , UnitT
                                                )
import           Inventory.Database.Utils       ( defaultJSONOptions )

data ItemT f = Item
  { _itemId       :: Columnar f (SqlSerial Int32)
  , _itemCount    :: Columnar f Int
  , _itemLocation :: PrimaryKey LocationT f
  , _itemUnit     :: PrimaryKey UnitT f
  }
  deriving stock Generic
instance Beamable ItemT

type Item = ItemT Identity
deriving stock instance Show Item
deriving stock instance Eq Item
instance FromJSON Item where
  parseJSON = genericParseJSON defaultJSONOptions
instance ToJSON Item where
  toJSON = genericToJSON defaultJSONOptions

type ItemID = PrimaryKey ItemT Identity
instance Table ItemT where
  data PrimaryKey ItemT f = ItemId (Columnar f (SqlSerial Int32))
    deriving stock Generic
    deriving anyclass Beamable
  primaryKey = ItemId . _itemId

Item (LensFor itemId) (LensFor itemCount) (LocationId (LensFor itemLocation)) (UnitId (LensFor itemUnit)) = tableLenses
