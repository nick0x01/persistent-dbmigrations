{-# LANGUAGE OverloadedStrings, QuasiQuotes, TypeSynonymInstances, FlexibleInstances #-}

module Database.Persist.Schema.Migrations.Persist
  ( getBootstrapMigration
  , isBootstrapped
  , applyMigration
  , revertMigration
  , getMigrations
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class
import Data.List.Split (endBy)
import Data.Maybe
import Database.Persist
import Database.Persist.Sql hiding (Migration)
import Data.Text hiding (map)
import Text.Shakespeare.Text
import Database.Schema.Migrations.Migration (Migration(..), newMigration)
import Database.Schema.Migrations.Backend (rootMigrationName)

migrationTableName :: String
migrationTableName = "installed_migrations"

migrationIdColName :: String
migrationIdColName = "migration_id"

getBootstrapMigration :: SqlPersistM Migration
getBootstrapMigration = do
  m <- liftIO $ newMigration rootMigrationName
  return $ m { mApply = unpack [st|CREATE TABLE #{migrationTableName} (#{migrationIdColName} TEXT)|]
             , mRevert = Just $ unpack [st|DROP TABLE #{migrationTableName}|]
             , mDesc = Just "Migration table installation"
             }

isBootstrapped :: SqlPersistM Bool
isBootstrapped = do
  -- This request will work only for MSSQL
  res <- rawSql "SELECT table_name FROM information_schema.tables" [] >>= return . map unSingle
  return $ elem (pack migrationTableName) res

applyMigration :: Migration -> SqlPersistM ()
applyMigration m = do
  let queryList = endBy ";" (mApply m)
  mapM (\q -> rawExecute (pack q) []) queryList
  rawExecute [st|INSERT INTO #{migrationTableName} (#{migrationIdColName}) VALUES (?)|] [toPersistValue (mId m)]

revertMigration :: Migration -> SqlPersistM ()
revertMigration m = do
  let revert = mRevert m
  when (isJust revert) $ do
    let queryList = endBy ";" (fromJust revert)
    mapM (\q -> rawExecute (pack q) []) queryList
    return ()
  rawExecute [st|DELETE FROM #{migrationTableName} WHERE #{migrationIdColName} = ?|] [toPersistValue (mId m)]

getMigrations :: SqlPersistM [String]
getMigrations =
  rawSql [st|SELECT #{migrationIdColName} FROM #{migrationTableName}|] [] >>= return . map unSingle
