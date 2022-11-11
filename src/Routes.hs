{-# LANGUAGE OverloadedStrings #-}

module Routes where

import Data.Text (pack)
import Database
import Model (Unit)
import Web.Scotty (ScottyM, get, html, json, param)

getHomeR :: ScottyM ()
getHomeR =
  get "/home" $ do
    html $
      mconcat
        [ "<h1> Welcome to /home! </h1>",
          "<h1> Currently supported Terran data at the following routes :- </h1>",
          "<p> <i> /terran/units/all <br>",
          "<i> /terran/units/named/:name <br></i>",
          "<i> /terran/units/from/barracks <br></i>",
          "<i> /terran/units/from/factory <br></i>",
          "<i> /terran/units/from/starport <br></i>",
          "<i> /terran/units/type/biological <br></i>",
          "<i> /terran/units/type/mechanical <br></i>",
          "<i> /terran/units/type/psionic <br></i>",
          "<i> /terran/units/type/summoned </i> </p>"
        ]

getAllUnitsR :: ScottyM ()
getAllUnitsR =
  get "/terran/units/all" $ do
    units <- withDB getAllUnits
    json (units :: [Unit])

getUnitNameR :: ScottyM ()
getUnitNameR =
  get "/terran/units/named/:name" $ do
    name <- param "name"
    let input = pack name
    units <- withDB $ getUnitWithName input
    json (units :: [Unit])

getBarracksUnitsR :: ScottyM ()
getBarracksUnitsR =
  get "/terran/units/from/barracks" $ do
    units <- withDB getBarracksUnits
    json (units :: [Unit])

getFactoryUnitsR :: ScottyM ()
getFactoryUnitsR =
  get "/terran/units/from/factory" $ do
    units <- withDB getFactoryUnits
    json (units :: [Unit])

getStarportUnitsR :: ScottyM ()
getStarportUnitsR =
  get "/terran/units/from/starport" $ do
    units <- withDB getStarportUnits
    json (units :: [Unit])

getBiologicalUnitsR :: ScottyM ()
getBiologicalUnitsR =
  get "/terran/units/type/biological" $ do
    units <- withDB getBiologicalUnits
    json (units :: [Unit])

getMechanicalUnitsR :: ScottyM ()
getMechanicalUnitsR =
  get "/terran/units/type/mechanical" $ do
    units <- withDB getMechanicalUnits
    json (units :: [Unit])

getPsionicUnitsR :: ScottyM ()
getPsionicUnitsR =
  get "/terran/units/type/psionic" $ do
    units <- withDB getPsionicUnits
    json (units :: [Unit])

getSummonedUnitsR :: ScottyM ()
getSummonedUnitsR =
  get "/terran/units/type/summoned" $ do
    units <- withDB getSummonedUnits
    json (units :: [Unit])
