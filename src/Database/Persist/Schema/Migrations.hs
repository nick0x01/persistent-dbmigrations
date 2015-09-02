module Database.Persist.Schema.Migrations
  ( createNewMigration
  , ensureBootstrappedBackend
  , migrationsToApply
  , migrationsToRevert
  , missingMigrations
  ) where

import qualified Data.Set as Set
import Data.Maybe
import Database.Persist.Sql hiding (Migration)

import Database.Schema.Migrations.Dependencies
import qualified Database.Schema.Migrations.Store as S
import Database.Schema.Migrations.Migration
import Database.Persist.Schema.Migrations.Persist

missingMigrations :: S.StoreData -> SqlPersistM [String]
missingMigrations storeData = do
  let storeMigrationNames = map mId $ S.storeMigrations storeData
  backendMigrations <- getMigrations
  return $ Set.toList $ Set.difference
         (Set.fromList storeMigrationNames)
         (Set.fromList backendMigrations)

createNewMigration :: S.MigrationStore -> String -> [String] -> IO (Either String Migration)
createNewMigration store name deps = do
  available <- S.getMigrations store
  case name `elem` available of
    True -> do
      fullPath <- S.fullMigrationName store name
      return $ Left $ "Migration " ++ (show fullPath) ++ " already exists"
    False -> do
      let new = newMigration name
      let newWithDefaults = new { mDesc = Just "(Description here.)"
                                , mApply = "(Apply SQL here.)"
                                , mRevert = Just "(Revert SQL here.)"
                                , mDeps = deps
                                }
      S.saveMigration store newWithDefaults
      return $ Right newWithDefaults

ensureBootstrappedBackend :: SqlPersistM ()
ensureBootstrappedBackend = do
  bsStatus <- isBootstrapped
  case bsStatus of
    True -> return ()
    False -> getBootstrapMigration >>= applyMigration

migrationsToApply :: S.StoreData -> Migration -> SqlPersistM [Migration]
migrationsToApply storeData migration = do
  let graph = S.storeDataGraph storeData
  allMissing <- missingMigrations storeData
  let deps = (dependencies graph $ mId migration) ++ [mId migration]
      namesToInstall = [ e | e <- deps, e `elem` allMissing ]
      loadedMigrations = catMaybes $ map (S.storeLookup storeData) namesToInstall
  return loadedMigrations

migrationsToRevert :: S.StoreData -> Migration -> SqlPersistM [Migration]
migrationsToRevert storeData migration = do
  let graph = S.storeDataGraph storeData
  allInstalled <- getMigrations
  let rDeps = (reverseDependencies graph $ mId migration) ++ [mId migration]
      namesToRevert = [ e | e <- rDeps, e `elem` allInstalled ]
      loadedMigrations = catMaybes $ map (S.storeLookup storeData) namesToRevert
  return loadedMigrations
