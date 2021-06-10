{- |
Copyright: (c) 2021 Tim Millican
SPDX-License-Identifier: MIT
Maintainer: Tim Millican <reyu@reyuzenfold.com>

-}

-- This is disabled for the Lens definitions
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Inventory.Database
  ( -- * Database definitions
    InventoryDb
  , inventoryDb
    -- ** Database table lenses
  , items
  , locations
  , units
  , unitConversions
    -- * Connection functions
  , withBeam
  , withDBConnection
  )
where

import           Configuration.Dotenv       (defaultConfig, loadFile)
import           Control.Exception
import           Database.Beam              (Database, DatabaseSettings, TableEntity, TableLens (TableLens), dbLenses,
                                             defaultDbSettings)
import           Database.Beam.Postgres     (Pg, runBeamPostgres, runBeamPostgresDebug)
import           Database.PostgreSQL.Simple (ConnectInfo (..), Connection, close, connectPostgreSQL,
                                             postgreSQLConnectionString)
import           System.Directory           (doesFileExist)
import           System.Environment         (lookupEnv)

import           Inventory.Database.Tables

-- | Database Definition
data InventoryDb f
  = InventoryDb
      { _inventoryItem :: f (TableEntity ItemT)
      , _inventoryLocation :: f (TableEntity LocationT)
      , _inventoryUnit :: f (TableEntity UnitT)
      , _inventoryUnitConversion :: f (TableEntity UnitConversionT)
      }
  deriving stock Generic
  deriving anyclass (Database be)

inventoryDb :: DatabaseSettings be InventoryDb
inventoryDb = defaultDbSettings

InventoryDb (TableLens items) (TableLens locations) (TableLens units) (TableLens unitConversions)
               = dbLenses

-- | Simple bracket wrapper to ensure database connections are closed
withDBConnection :: (Connection -> IO a) -> IO a
withDBConnection = bracket openPostgres close

-- | Wrapper for runBeamPostgres that enables debug printing to STDOUT if API_DEBUG set in env
withBeam :: Pg a -> IO a
withBeam f = do
  isDebug <- lookupEnv ("API_DEBUG" :: String)
  let run = case isDebug of
        Nothing -> runBeamPostgres
        Just _  -> runBeamPostgresDebug (putStrLn :: String -> IO ())
  withDBConnection $ \c -> run c f

-- | Connect to PostgreSQL using environment variables for configuration
openPostgres :: IO Connection
openPostgres = do
  e <- doesFileExist ".env"
  when e $ void (loadFile defaultConfig)

  hostname <- lookupEnv ("API_DB_HOST" :: String)
  port     <- lookupEnv ("API_DB_PORT" :: String)
  username <- lookupEnv ("API_DB_USER" :: String)
  password <- lookupEnv ("API_DB_PASS" :: String)
  database <- lookupEnv ("API_DB_DB"   :: String)

  let port' = case (readEither (fromMaybe "5432" port) :: Either Text Word16) of
        Right p -> p
        Left _  -> error "Could not recognise PORT"

  let connectionInfo =
        ConnectInfo
          (fromMaybe "localhost" hostname)
          port'
          (fromMaybe "canidcomics" username)
          (fromMaybe "" password)
          (fromMaybe "canidcomics" database)
  connectPostgreSQL $ postgreSQLConnectionString connectionInfo
