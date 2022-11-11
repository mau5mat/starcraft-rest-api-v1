{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Model (Unit, UnitType (Biological, Mechanical, Psionic, Summoned), Building (Barracks, Factory, Starport), allUnits) where

import Data.Aeson.Types (ToJSON)
import Data.Text (Text)
import Database.Selda (SqlRow, SqlType)
import GHC.Generics (Generic)

data Unit = Unit
  { name :: Text,
    unitType :: UnitType,
    builtFrom :: Building,
    mineralCost :: Int,
    gasCost :: Int,
    supply :: Int,
    hotkey :: Text
  }
  deriving (Generic, Show)

instance SqlRow Unit

instance ToJSON Unit

data UnitType
  = Biological
  | Mechanical
  | Psionic
  | Summoned
  deriving (Show, Read, Bounded, Enum, Eq, Generic)

instance SqlType UnitType

instance ToJSON UnitType

data Building
  = CommandCenter
  | Barracks
  | Factory
  | Starport
  deriving (Show, Read, Bounded, Enum, Eq, Generic)

instance SqlType Building

instance ToJSON Building

allUnits :: [Unit]
allUnits =
  [ scv,
    marine,
    marauder,
    reaper,
    ghost,
    hellion,
    hellbat,
    siegeTank,
    cyclone,
    widowMine,
    thor,
    viking,
    medivac,
    liberator,
    raven,
    banshee,
    battlecruiser,
    autoTurret
  ]

-- Command Center
scv :: Unit
scv = Unit "SCV" Mechanical CommandCenter 50 0 1 "S"

-- Barracks
marine :: Unit
marine = Unit "Marine" Biological Barracks 50 0 1 "A"

marauder :: Unit
marauder = Unit "Marauder" Biological Barracks 100 25 2 "D"

reaper :: Unit
reaper = Unit "Reaper" Biological Barracks 50 50 1 "R"

ghost :: Unit
ghost = Unit "Ghost" Psionic Barracks 150 125 2 "G"

-- Factory
hellion :: Unit
hellion = Unit "Hellion" Mechanical Factory 100 0 2 "E"

hellbat :: Unit
hellbat = Unit "Hellbat" Mechanical Factory 100 0 2 "R"

siegeTank :: Unit
siegeTank = Unit "Siege Tank" Mechanical Factory 150 125 3 "S"

cyclone :: Unit
cyclone = Unit "Cyclone" Mechanical Factory 150 100 3 "N"

widowMine :: Unit
widowMine = Unit "Widow Mine" Mechanical Factory 75 25 2 "D"

thor :: Unit
thor = Unit "Thor" Mechanical Factory 300 200 6 "T"

-- Starport
viking :: Unit
viking = Unit "Viking" Mechanical Starport 150 75 2 "V"

medivac :: Unit
medivac = Unit "Medivac" Mechanical Starport 100 100 2 "D"

liberator :: Unit
liberator = Unit "Liberator" Mechanical Starport 150 150 3 "N"

raven :: Unit
raven = Unit "Raven" Mechanical Starport 100 200 2 "R"

banshee :: Unit
banshee = Unit "Banshee" Mechanical Starport 150 100 3 "E"

battlecruiser :: Unit
battlecruiser = Unit "Battlecruiser" Mechanical Starport 400 300 6 "B"

-- Summoned
autoTurret :: Unit
autoTurret = Unit "Auto-Turret" Summoned Starport 0 0 0 "T"
