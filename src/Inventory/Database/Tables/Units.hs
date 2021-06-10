{- |
Module: Inventory.Database.Tables.Units
Description: 
Copyright: (c) 2021 Tim Millican
SPDX-License-Identifier: MIT
Maintainer: Tim Millican <reyu@reyuzenfold.com>
Stability: experimental

-}

-- This are set for the Lens definitions
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE ImpredicativeTypes         #-}
{-# LANGUAGE ExplicitNamespaces         #-}

module Inventory.Database.Tables.Units
  ( -- * Primary Keys
    PrimaryKey(UnitId, UnitConversionId)
    -- * Units Table
  , UnitT
  , Unit
    -- ** Lenses
  , unitId
  , unitName
  , unitDescription
    -- * Unit Conversion Table
  , UnitConversionT
  , UnitConversion
    -- ** Lenses
  , unitFrom
  , unitTo
  , unitMultiplier
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

data UnitT f = Unit
  { _unitId          :: Columnar f (SqlSerial Int32)
  , _unitName        :: Columnar f Text
  , _unitDescription :: Columnar f (Maybe Text)
  }
  deriving stock Generic
instance Beamable UnitT
instance Table UnitT where
  data PrimaryKey UnitT f = UnitId (Columnar f (SqlSerial Int32))
    deriving stock Generic
    deriving anyclass Beamable
  primaryKey = UnitId <$> _unitId

type Unit = UnitT Identity
deriving stock instance Show Unit
deriving stock instance Eq Unit
instance FromJSON Unit where
  parseJSON = genericParseJSON defaultJSONOptions
instance ToJSON Unit where
  toJSON = genericToJSON defaultJSONOptions

type UnitId = PrimaryKey UnitT Identity
deriving stock instance Show UnitId
deriving stock instance Eq UnitId
instance FromJSON UnitId where
  parseJSON = genericParseJSON defaultJSONOptions
instance ToJSON UnitId where
  toJSON = genericToJSON defaultJSONOptions

Unit (LensFor unitId) (LensFor unitName) (LensFor unitDescription) = tableLenses

-- Unit Conversion

data UnitConversionT f = UnitConversion
  { _unitConversionFrom       :: PrimaryKey UnitT f
  , _unitConversionTo         :: PrimaryKey UnitT f
  , _unitConversionMultiplier :: Columnar f Int
  }
  deriving stock Generic
instance Beamable UnitConversionT
instance Table UnitConversionT where
  data PrimaryKey UnitConversionT f = UnitConversionId (PrimaryKey UnitT f) (PrimaryKey UnitT f)
    deriving stock Generic
    deriving anyclass Beamable
  primaryKey = UnitConversionId <$> _unitConversionFrom <*> _unitConversionTo

type UnitConversion = UnitConversionT Identity
deriving stock instance Show UnitConversion
deriving stock instance Eq UnitConversion
instance FromJSON UnitConversion where
  parseJSON = genericParseJSON defaultJSONOptions
instance ToJSON UnitConversion where
  toJSON = genericToJSON defaultJSONOptions

type UnitConversionId = PrimaryKey UnitConversionT Identity
deriving stock instance Show UnitConversionId
deriving stock instance Eq UnitConversionId
instance FromJSON UnitConversionId where
  parseJSON = genericParseJSON defaultJSONOptions
instance ToJSON UnitConversionId where
  toJSON = genericToJSON defaultJSONOptions

UnitConversion (UnitId (LensFor unitFrom)) (UnitId (LensFor unitTo)) (LensFor unitMultiplier) = tableLenses
