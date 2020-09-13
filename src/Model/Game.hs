{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Model.Game where

import Data.Aeson
import Data.Vector (Vector)
import Database.Persist.TH
import GHC.Generics
import Prelude

data PlayerType = Owner | Guest deriving (Generic, Show, Read, Eq)

instance ToJSON PlayerType

instance FromJSON PlayerType

-- Setup True False --> Only the owner has setup their battlefield
-- Setup False False --> No one setup their battlefield
data GameState = Lobby | Setup Bool Bool | OwnerTurn | GuestTurn | Finished | Cancelled
  deriving (Generic, Show, Read, Eq)

instance ToJSON GameState

instance FromJSON GameState

-- List of ship names
data Ship = Carrier | Battleship | Destroyer | Submarine | PatrolBoat deriving (Show, Eq, Enum)

-- we're using the enum value of the ship when serializing to JSON
-- made sense in the beginning, now I think the name itself would also be okay
instance ToJSON Ship where
  toJSON a = toJSON $ fromEnum a

instance FromJSON Ship where
  parseJSON a = do
    num :: Int <- parseJSON a
    return $ toEnum num

-- every different ship has a different length!
shipLength :: Ship -> Int
shipLength Carrier = 5
shipLength Battleship = 4
shipLength Destroyer = 3
shipLength Submarine = 3
shipLength PatrolBoat = 2

-- fst: Did the owner hit this field?
-- snd: DId the guest hit this field?
type Field = (Bool, Bool)

-- The battlefield is a 2D Vector of "Fields"
type Battlefield = Vector (Vector Field)

-- A ship can be placed vertical or horizontal
data ShipDirection = RightWards | DownWards deriving (Show, Eq, Generic)

instance FromJSON ShipDirection

instance ToJSON ShipDirection

-- The ship itself, the row, the column and in which direction it's pointing
type ShipPlacement = (Ship, Int, Int, ShipDirection)

-- fst: all ship placements of the owner
-- snd: all ship placements of the guest
type ShipPlacements = ([ShipPlacement], [ShipPlacement])

-- hits: All hits that have been made
-- ships: All visible ships that are on the battlefield (e.g visible for the enemy only if completely sunken)
data BattlefieldView = BattlefieldView
  { hits :: Vector (Int, Int, Bool),
    ships :: [(ShipPlacement, Bool)]
  }
  deriving (Show, Eq, Generic)

instance ToJSON BattlefieldView

-- every player has a different view. I see all my ships, while the enemy should only see completely sunken ships.
-- so a player has two views on the battlefield: One from himself and one from his enemy
data BattlefieldStateView = BattlefieldStateView
  { you :: BattlefieldView,
    enemy :: BattlefieldView
  }
  deriving (Show, Eq, Generic)

instance ToJSON BattlefieldStateView

-- makes the types useable in persistent, very neat!
derivePersistField "GameState"
derivePersistField "PlayerType"
