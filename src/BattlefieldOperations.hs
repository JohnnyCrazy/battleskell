{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module BattlefieldOperations (validateBattlefieldInput, shipCoordinates, validateHit, battlefieldStateView) where

import Control.Lens
import qualified Data.List as L
import Database.Esqueleto.PostgreSQL.JSON (JSONB (JSONB))
import Import
import Model.Game
import qualified Model.Game as G

-- this pseudo battlefield is used to see if ships overlap by using a counter, starting at 0
type PseudoBattlefied = [[Int]]

filterShipPlacements :: Ship -> [ShipPlacement] -> [ShipPlacement]
filterShipPlacements ship placements = filter (\(s, _, _, _) -> s == ship) placements

-- converts a ship placement to the following:
-- (start_coords, end_coords, all_coords_between)
-- so for example, a 3 length ship placements (_, 0, 0, RightWards) would result in:
-- ((0, 0), (0, 2), [(0,0), (0,1), (0, 2)])
shipCoordinates :: ShipPlacement -> ((Int, Int), (Int, Int), [(Int, Int)])
shipCoordinates (ship, row, col, dir) = ((row, col), (endRow, endCol), coords)
  where
    len = shipLength ship
    (endRow, endCol) = case dir of
      RightWards -> (row, col + len - 1)
      DownWards -> (row + len - 1, col)
    coords = [(rowCoord, colCoord) | rowCoord <- [row .. endRow], colCoord <- [col .. endCol]]

-- by checking if the pseudo battlefield has a maximum value of 1 at each field, we know no ships overlap
validatePseudoBattlefied :: PseudoBattlefied -> Either String ()
validatePseudoBattlefied battlefield = case all (\x -> x <= 1) (L.concat battlefield) of
  True -> Right ()
  False -> Left "The placements of the ships overlap"

-- iterate through all ship placements and place them on the pseudo battlefield
-- when there are no more placements, validate via validatePseudoBattlefied
validateShipPlacements_ :: [ShipPlacement] -> PseudoBattlefied -> Either String ()
validateShipPlacements_ [] battlefield = validatePseudoBattlefied battlefield
validateShipPlacements_ (placement : xs) battlefield = do
  validateStartEnd
  -- recursion with the placements added to the pseudo battlefield
  validateShipPlacements_ xs placedShips
  where
    -- convert to all coordinates so we can place them on the pseudo battlefield
    ((row, col), (endRow, endCol), coords) = shipCoordinates placement

    -- basic validation of start and end coordinates
    validStart = row >= 0 && row <= 9 && col >= 0 && col <= 9
    validEnd = endRow >= 0 && endRow <= 9 && endCol >= 0 && endCol <= 9
    validateStartEnd = if validStart && validEnd then Right () else Left "Start & End of the ship is invalid"

    -- place all coords on the pseudo battlefield
    placedShips =
      foldr
        ( \(shipRow, shipCol) bf -> do
            -- using lenses to increment the value at (row,col) of the 2D list by one
            bf & ix shipRow . ix shipCol %~ (+ 1)
        )
        battlefield
        coords

-- helper function to create the pseudo battlefield before iterating through the ship placements
validateShipPlacements :: [ShipPlacement] -> Either String ()
validateShipPlacements placements = validateShipPlacements_ placements battlefield
  where
    battlefield :: PseudoBattlefied = [[0 | _ <- [0 :: Int .. 9]] | _ <- [0 :: Int .. 9]]

validateShipCount :: Int -> [ShipPlacement] -> Either String ()
validateShipCount amount placements
  | length placements == amount = Right ()
  | otherwise = Left "A ship did not have correct amount of placements"

validateTotalLength :: Int -> [ShipPlacement] -> Either String ()
validateTotalLength expectedLen placements
  | len == expectedLen = Right ()
  | otherwise = Left $ "Expected " ++ show expectedLen ++ " positions, got " ++ show len
  where
    len = length placements

-- either monad helps to return early if there is an error
validateBattlefieldInput :: [ShipPlacement] -> Either String ()
validateBattlefieldInput placements = do
  validateTotalLength 5 placements
  validateShipCount 1 $ filterShipPlacements Carrier placements
  validateShipCount 1 $ filterShipPlacements Battleship placements
  validateShipCount 1 $ filterShipPlacements Destroyer placements
  validateShipCount 1 $ filterShipPlacements Submarine placements
  validateShipCount 1 $ filterShipPlacements PatrolBoat placements
  validateShipPlacements placements
  return ()

validateHit :: Game -> PlayerType -> Int -> Int -> Either String (Bool, Bool)
validateHit game pType row col = do
  (ownerHit, guestHit) <- maybe (Left "row/col out of bounds") (\x -> return x) mfield
  if alreadyHit ownerHit guestHit then Left ("This cell is already hit!" :: String) else return (didHitShip, allShipsSunk)
  where
    JSONB battlefield = gameBattlefield game
    JSONB (ownerShips, guestShips) = gameShipPlacements game
    mfield = battlefield ^? ix row . ix col

    -- ugly but works, update specific field via lens and setter function
    allShipsSunk = case pType of
      Guest -> all (isShipSunk (listBattlefieldHits snd $ battlefield & ix row . ix col %~ (\(_, _) -> (False, True)))) ownerShips
      Owner -> all (isShipSunk (listBattlefieldHits fst $ battlefield & ix row . ix col %~ (\(_, _) -> (True, False)))) guestShips

    didHitShip = case pType of
      Guest -> any (isShipHit (row, col)) ownerShips
      Owner -> any (isShipHit (row, col)) guestShips

    alreadyHit ownerHit guestHit = case pType of
      Guest -> guestHit
      Owner -> ownerHit

isCoordsBetween :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
isCoordsBetween (startRow, startCol) (endRow, endCol) (row, col) =
  row >= startRow && row <= endRow && col >= startCol && col <= endCol

isShipHit :: (Int, Int) -> ShipPlacement -> Bool
isShipHit pos (shipId, shipRow, shipCol, dir) = do
  isCoordsBetween startPos endPos pos
  where
    len = shipLength shipId
    startPos = (shipRow, shipCol)
    endPos = case dir of
      RightWards -> (shipRow, shipCol + len - 1)
      DownWards -> (shipRow + len - 1, shipCol)

isShipSunk :: Vector (Int, Int) -> ShipPlacement -> Bool
isShipSunk hits (shipId, shipRow, shipCol, dir) =
  all (\coord -> elem coord hits) coords
  where
    len = shipLength shipId
    (startRow, startCol) = (shipRow, shipCol)
    (endRow, endCol) = case dir of
      RightWards -> (shipRow, shipCol + len - 1)
      DownWards -> (shipRow + len - 1, shipCol)
    coords = [(rowCoord, colCoord) | rowCoord <- [startRow .. endRow], colCoord <- [startCol .. endCol]]

-- get a list of row/col combinations, which have been hit.
-- the selector is used to filter either owner or guest hits.
-- since a vector of vectors is used for the battlefields, it's not easy to filter and preserve the row/column index
listBattlefieldHits :: (G.Field -> Bool) -> Battlefield -> Vector (Int, Int)
listBattlefieldHits selector battlefield =
  listOfCoordsOnly
  where
    cellsWithCoords = concat $ imap (\rowI row -> imap (\colI val -> (rowI, colI, val)) row) battlefield
    filteredCells = filter (\(_, _, val) -> selector val) cellsWithCoords
    listOfCoordsOnly = map (\(row, col, _) -> (row, col)) filteredCells

-- could've been made simpler, lot of repeated code
battlefieldStateView :: PlayerType -> Battlefield -> ShipPlacements -> BattlefieldStateView
battlefieldStateView Owner battlefield (placements) = do
  BattlefieldStateView (BattlefieldView takenHits mySunkenShips) (BattlefieldView doneHits enemySunkenShips)
  where
    myPlacements = fst placements
    enemyPlacements = snd placements

    hitsAtMyField = listBattlefieldHits snd battlefield
    takenHits = map (\(row, col) -> (row, col, any (isShipHit (row, col)) myPlacements)) hitsAtMyField
    mySunkenShips = map (\ship -> (ship, isShipSunk hitsAtMyField ship)) myPlacements

    hitsAtEnemyField = listBattlefieldHits fst battlefield
    doneHits = map (\(row, col) -> (row, col, any (isShipHit (row, col)) enemyPlacements)) $ hitsAtEnemyField
    enemySunkenShips = map (\ship -> (ship, True)) $ filter (\ship -> isShipSunk hitsAtEnemyField ship) enemyPlacements
battlefieldStateView Guest battlefield (placements) = do
  BattlefieldStateView (BattlefieldView takenHits mySunkenShips) (BattlefieldView doneHits enemySunkenShips)
  where
    myPlacements = snd placements
    enemyPlacements = fst placements

    hitsAtMyField = listBattlefieldHits fst battlefield
    takenHits = map (\(row, col) -> (row, col, any (isShipHit (row, col)) myPlacements)) $ hitsAtMyField
    mySunkenShips = map (\ship -> (ship, isShipSunk hitsAtMyField ship)) myPlacements

    hitsAtEnemyField = listBattlefieldHits snd battlefield
    doneHits = map (\(row, col) -> (row, col, any (isShipHit (row, col)) enemyPlacements)) $ hitsAtEnemyField
    enemySunkenShips = map (\ship -> (ship, True)) $ filter (\ship -> isShipSunk hitsAtEnemyField ship) enemyPlacements
