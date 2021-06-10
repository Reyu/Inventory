{- |
Module: Inventory.Database.Tables
Description: All tables under the Inventory Database
Copyright: (c) 2021 Tim Millican
SPDX-License-Identifier: MIT
Maintainer: Tim Millican <reyu@reyuzenfold.com>
Stability: experimental

Re-export all tables. Will likely also include
related helper functions at a later point.
-}

module Inventory.Database.Tables
  ( module Items
  , module Locations
  , module Units
  ) where

import           Inventory.Database.Tables.Items
                                               as Items
import           Inventory.Database.Tables.Locations
                                               as Locations
import           Inventory.Database.Tables.Units
                                               as Units
