{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Database where

import Data.Text (Text)
import Database.Selda
import Database.Selda.SQLite (SQLite, withSQLite)
import Model

-- Add allUnits into Database
addUnitsToDb :: IO ()
addUnitsToDb = withSQLite dbFile $ do
  -- createTable only gets used once to generate the Table
  -- dropTable unitTable
  createTable unitTable
  insert_ unitTable allUnits

-- Database file
dbFile :: FilePath
dbFile = "terran.sqlite"

-- Database Helper
withDB :: MonadIO m => SeldaT SQLite IO a -> m a
withDB tryQuery = liftIO $ withSQLite dbFile $ do
  tryCreateTable unitTable
  tryQuery

-- Create Table
unitTable :: Table Unit
unitTable = table "units" [#name :- primary]

-- Delete Table
clearUnitTable :: MonadSelda m => m ()
clearUnitTable = deleteFrom_ unitTable (const true)

-- Insert
insertUnitToDb :: Unit -> SeldaM b Int
insertUnitToDb unit = withSQLite dbFile $ do
  insert unitTable [unit]

-- Delete
deleteUnitWithName :: Text -> SeldaM b Int
deleteUnitWithName name = withSQLite dbFile $ do
  deleteFrom unitTable (\unit -> unit ! #name .== literal name)

-- Get
getAllUnits :: MonadSelda m => m [Unit]
getAllUnits = do
  query $ select unitTable

getUnitWithName :: MonadSelda m => Text -> m [Unit]
getUnitWithName name = do
  query $ do
    unit <- select unitTable
    restrict (unit ! #name .== literal name)
    return unit

getBarracksUnits :: MonadSelda m => m [Unit]
getBarracksUnits = do
  query $ do
    unit <- select unitTable
    restrict (is #builtFrom Barracks unit)
    return unit

getFactoryUnits :: MonadSelda m => m [Unit]
getFactoryUnits = do
  query $ do
    unit <- select unitTable
    restrict (is #builtFrom Barracks unit)
    return unit

getStarportUnits :: MonadSelda m => m [Unit]
getStarportUnits = do
  query $ do
    unit <- select unitTable
    restrict (is #builtFrom Barracks unit)
    return unit

getBiologicalUnits :: MonadSelda m => m [Unit]
getBiologicalUnits = do
  query $ do
    unit <- select unitTable
    restrict (is #unitType Biological unit)
    return unit

getMechanicalUnits :: MonadSelda m => m [Unit]
getMechanicalUnits = do
  query $ do
    unit <- select unitTable
    restrict (is #unitType Mechanical unit)
    return unit

getPsionicUnits :: MonadSelda m => m [Unit]
getPsionicUnits = do
  query $ do
    unit <- select unitTable
    restrict (is #unitType Psionic unit)
    return unit

getSummonedUnits :: MonadSelda m => m [Unit]
getSummonedUnits = do
  query $ do
    unit <- select unitTable
    restrict (is #unitType Summoned unit)
    return unit
