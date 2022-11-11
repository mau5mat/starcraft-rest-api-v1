module Main where

import Database
import Routes
import Web.Scotty (scotty)

{-
   - Connect to SQLite database                                                  ✅
   - Run operation to bulk store a List of [Unit]'s in the database              ✅
   - Create actions to operate on the data inside Database                       ✅
   - Transform this Data into JSON                                               ✅
   - Serve relevant JSON data to particular Endpoints                            ✅
   - Future features -> Add Building data? --> Provide richer type information?  ❌
-}

main :: IO ()
main = do
  -- addUnitsToDb

  scotty 3000 $ do
    getHomeR
    getAllUnitsR
    getUnitNameR
    getBarracksUnitsR
    getFactoryUnitsR
    getStarportUnitsR
    getBiologicalUnitsR
    getMechanicalUnitsR
    getPsionicUnitsR
    getSummonedUnitsR
