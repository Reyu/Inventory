{- |
Module: Inventory.Database.Tables.Locations
Description: 
Copyright: (c) 2021 Tim Millican
SPDX-License-Identifier: MIT
Maintainer: Tim Millican <reyu@reyuzenfold.com>
Stability: experimental

-}

-- This is disabled for the Lens definitions
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-#  LANGUAGE ImpredicativeTypes #-}

module Inventory.Database.Tables.Locations
  ( -- * Primary Key
    PrimaryKey(LocationId)
    -- * Tables
  , LocationT
    -- * Lenses
  , locationId
  , locationName
  , locationDescription
  , locationParent
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
import           Database.Beam.Schema           ( Nullable )
import           Inventory.Database.Utils       ( defaultJSONOptions )

data LocationT f = Location
  { _locationId          :: Columnar f (SqlSerial Int32)
  , _locationName        :: Columnar f Text
  , _locationDescription :: Columnar f (Maybe Text)
  , _locationParent      :: PrimaryKey LocationT (Nullable f)
  }
  deriving stock Generic
instance Beamable LocationT
instance Table LocationT where
  data PrimaryKey LocationT f = LocationId (Columnar f (SqlSerial Int32))
    deriving stock Generic
    deriving anyclass Beamable
  primaryKey = LocationId <$> _locationId

type Location = LocationT Identity
deriving stock instance Show Location
deriving stock instance Eq Location
instance FromJSON Location where
  parseJSON = genericParseJSON defaultJSONOptions
instance ToJSON Location where
  toJSON = genericToJSON defaultJSONOptions
deriving stock instance Show (PrimaryKey LocationT (Nullable Identity))
deriving stock instance Eq (PrimaryKey LocationT (Nullable Identity))
instance FromJSON (PrimaryKey LocationT (Nullable Identity)) where
  parseJSON = genericParseJSON defaultJSONOptions
instance ToJSON (PrimaryKey LocationT (Nullable Identity)) where
  toJSON = genericToJSON defaultJSONOptions

type LocationId = PrimaryKey LocationT Identity
deriving stock instance Show LocationId
deriving stock instance Eq LocationId
instance FromJSON LocationId where
  parseJSON = genericParseJSON defaultJSONOptions
instance ToJSON LocationId where
  toJSON = genericToJSON defaultJSONOptions

Location (LensFor locationId) (LensFor locationName) (LensFor locationDescription) (LocationId (LensFor locationParent))
  = tableLenses
