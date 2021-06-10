{- |
Copyright: (c) 2021 Tim Millican
SPDX-License-Identifier: MIT
Maintainer: Tim Millican <reyu@reyuzenfold.com>

-}

module Inventory.Database.Utils
  ( defaultJSONOptions
  ) where


import           Data.Aeson                     ( Options
                                                , camelTo2
                                                , defaultOptions
                                                , fieldLabelModifier
                                                , omitNothingFields
                                                )
import           Data.Char                      ( isUpper )

--
-- | Function to provide default JSON conversion options
--   drops the `_classname` prefix, and converts from camel to snake case
defaultJSONOptions :: Options
defaultJSONOptions =
  defaultOptions { omitNothingFields = True, fieldLabelModifier = camelTo2 '_' . dropWhile (not . isUpper) }
