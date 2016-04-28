{-# LANGUAGE OverloadedStrings, QuasiQuotes, TypeSynonymInstances, FlexibleInstances #-}

module Database.Persist.Schema.Migrations.Persist
  ( getBootstrapMigration
  , isBootstrapped
  , applyMigration
  , revertMigration
  , getMigrations
  , migrationTableName
  , migrationIdColName
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class
import Data.List.Split (splitWhen)
import Data.Maybe
import Database.Persist
import Database.Persist.Sql hiding (Migration)
import Text.Shakespeare.Text
import Database.Schema.Migrations.Migration (Migration(..), newMigration)
import Database.Schema.Migrations.Backend (rootMigrationName)
import qualified Data.Text as T

migrationTableName :: String
migrationTableName = "installed_migrations"

migrationIdColName :: String
migrationIdColName = "migration_id"

getBootstrapMigration :: SqlPersistM Migration
getBootstrapMigration = do
  let m = newMigration rootMigrationName
  return $ m { mApply = T.unpack [st|CREATE TABLE #{migrationTableName} (#{migrationIdColName} TEXT)|]
             , mRevert = Just $ T.unpack [st|DROP TABLE #{migrationTableName}|]
             , mDesc = Just "Migration table installation"
             }

isBootstrapped :: SqlPersistM Bool
isBootstrapped = do
  -- This request will work only for MSSQL
  res <- map unSingle <$> rawSql "SELECT table_name FROM information_schema.tables" []
  return $ elem (T.pack migrationTableName) res

applyMigration :: Migration -> SqlPersistM ()
applyMigration m = do
  let queryList = splitStatements $ mApply m
  mapM_ (\q -> rawExecute q []) queryList
  rawExecute [st|INSERT INTO #{migrationTableName} (#{migrationIdColName}) VALUES (?)|] [toPersistValue (mId m)]

revertMigration :: Migration -> SqlPersistM ()
revertMigration m = do
  let revert = mRevert m
  when (isJust revert) $ do
    let queryList = splitStatements $ fromJust revert
    mapM_ (\q -> rawExecute q []) queryList
    return ()
  rawExecute [st|DELETE FROM #{migrationTableName} WHERE #{migrationIdColName} = ?|] [toPersistValue (mId m)]

splitStatements :: String -> [T.Text]
splitStatements = map T.unlines . splitWhen isSeparator . T.lines . T.pack
  where isSeparator x =
          let stripped = T.strip x
          in  stripped == ";" || stripped == "GO"

getMigrations :: SqlPersistM [String]
getMigrations =
  map unSingle <$> rawSql [st|SELECT #{migrationIdColName} FROM #{migrationTableName}|] []
