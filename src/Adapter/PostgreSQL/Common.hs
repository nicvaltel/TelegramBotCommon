{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Adapter.PostgreSQL.Common
  ( AppState,
    PG,
    DBConfig,
    readDBConfig,
    withAppState,
    withConn,
  )
where

import Configuration.Dotenv (parseFile)
import Control.Exception (bracket)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.RWS (MonadReader, asks)
import Data.Either.Combinators (maybeToRight)
import Data.Has (Has (getter))
import Data.Pool (Pool, defaultPoolConfig, destroyAllResources, newPool, withResource)
import Data.String (fromString)
import Database.PostgreSQL.Simple
  ( Connection,
    close,
    connectPostgreSQL,
    withTransaction,
  )
import Database.PostgreSQL.Simple.Migration
  ( MigrationCommand (MigrationDirectory, MigrationInitialization),
    MigrationResult (MigrationError),
    runMigrations,
  )
import Text.Printf (printf)
import UnliftIO (throwString)

type AppState = Pool Connection

type PG r m = (Has (Pool Connection) r, MonadReader r m, MonadIO m, MonadThrow m)

data DBConfig = DBConfig
  { dbHost :: String,
    dbPort :: Int,
    dbName :: String,
    dbUser :: String,
    dbPassword :: String,
    dbStripeCount :: Int,
    dbMaxOpenConnPerStripe :: Int,
    dbIdleConnTimeout :: Double
  }
  deriving (Show)

readDBConfig :: [(String, String)] -> Either String DBConfig
readDBConfig env = do
  dbHost <- maybeToRight "No Hostname defined" (lookup "POSTGRES_HOST" env)
  dbPort <- maybeToRight "No port number defined" (read <$> lookup "POSTGRES_PORT" env)
  dbName <- maybeToRight "No database name defined" (lookup "POSTGRES_DB" env)
  dbUser <- maybeToRight "No username defined" (lookup "POSTGRES_USER" env)
  dbPassword <- maybeToRight "No password defined" (lookup "POSTGRES_PASSWORD" env)
  dbStripeCount <- maybeToRight "No stripe count defined" (read <$> lookup "POSTGRES_STRIPE_COUNT" env)
  dbMaxOpenConnPerStripe <- maybeToRight "No max open connections per stripe defined" (read <$> lookup "POSTGRES_MAX_OPEN_CONN_PER_STRIPE" env)
  dbIdleConnTimeout <- maybeToRight "No stripe count defined" (read <$> lookup "POSTGRES_IDLE_CONN_TIMEOUT" env)
  pure DBConfig {dbHost, dbPort, dbName, dbUser, dbPassword, dbStripeCount, dbMaxOpenConnPerStripe, dbIdleConnTimeout}

migrate :: Pool Connection -> IO ()
migrate pool = withResource pool $ \conn -> do
  result <- withTransaction conn (runMigrations False conn cmds)
  case result of
    MigrationError err -> throwString err
    _ -> pure ()
  where
    cmds =
      [ MigrationInitialization,
        MigrationDirectory "migration/PostgreSQL/Migrations_00_Create_Tables",
        MigrationDirectory "migration/PostgreSQL/Migrations_01_Fill_Tables"
      ]

withPool :: DBConfig -> (Pool Connection -> IO a) -> IO a
withPool conf action = do
  bracket initPool cleanPool action
  where
    initPool = newPool $ defaultPoolConfig openConn close (dbIdleConnTimeout conf) (dbMaxOpenConnPerStripe conf)
    cleanPool = destroyAllResources
    openConn = connectPostgreSQL (fromString connectString)

    connectString :: String
    connectString = printf "host='%s' port=%d dbname='%s' user='%s' password='%s'" (dbHost conf) (dbPort conf) (dbName conf) (dbUser conf) (dbPassword conf)

withAppState :: DBConfig -> (AppState -> IO a) -> IO a
withAppState conf action =
  withPool conf $ \poolConnection -> do
    migrate poolConnection
    action poolConnection

withConn :: PG r m => (Connection -> IO a) -> m a
withConn action = do
  pool <- asks getter
  liftIO $ withResource pool (\conn -> action conn)
