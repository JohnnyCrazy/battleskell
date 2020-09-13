{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.GameState where

import BattlefieldOperations (battlefieldStateView, validateBattlefieldInput, validateHit)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Database.Esqueleto as ESQ
import Database.Esqueleto.PostgreSQL.JSON (JSONB (JSONB))
import Database.Persist.Postgresql
import Elo
import GameLog
import GameStateUpdater
import Import
import Model.Game
import Model.GameStateInputs
import Util
import Yesod.WebSockets

-- A player is either the Owner or the Guest
-- In the end, this was not a perfect solution because you need to check which type he is in a lot of operations.
playerType :: Game -> Key User -> PlayerType
playerType game userId
  | userId == gameOwnerId game = Owner
  | Just userId == gameGuestId game = Guest
  | True = error "Player Type can't be deduced"

-- make sure the game exists
-- this function can short-circuit via notFound
requireGame :: Key Game -> Handler (Entity Game)
requireGame gameId = do
  game <- runDB $ selectFirst [GameId ==. gameId] []
  case game of
    Nothing -> notFound
    Just g -> return g

-- similar to requireGame, checks if a game is in the correct state via a passed function
requireGameState :: MonadHandler m => Game -> (GameState -> Bool) -> m ()
requireGameState game stateChecker = case (stateChecker $ gameState game) of
  True -> return ()
  False ->
    -- also short circuits AFAIK
    sendStatusJSON status400 $
      object ["ok" .= False, "error" .= ("The game state is currently not suited for this operation" :: Text)]

-- Esqueleto/Persistent do not have support for JSONB as a function, so we need to do raw SQL here
-- 100% no SQL injection possible :)
-- also related: https://github.com/bitemyapp/esqueleto/issues/173
generateJSONBSetForShipPlacement :: String -> PlayerType -> [ShipPlacement] -> String
generateJSONBSetForShipPlacement existing pType ships =
  "jsonb_set(" ++ existing ++ ", '{" ++ pIndex ++ "}','" ++ (C.unpack $ encode ships) ++ "', false)"
  where
    pIndex = case pType of
      Owner -> "0"
      Guest -> "1"

-- get the amount of games a user has played
gameCount :: Key User -> Handler Int
gameCount userId = runDB $ count ([GameOwnerId ==. userId] ||. [GameGuestId ==. Just userId])

-- update the elo of all participating users
finishGame :: Entity Game -> Entity User -> Handler ()
finishGame (Entity gameId game) (Entity winnerId winner) = do
  newGameLog gameId (Just pType) (userUsername winner ++ " won the game! Congratulations!")
  loserM <- runDB $ selectFirst loserFilter []
  case loserM of
    Just (Entity loserId loser) -> do
      winnerGameCount <- gameCount winnerId
      loserGameCount <- gameCount loserId
      runDB $ do
        updateWhere [UserId ==. winnerId] [UserElo =. (newElo winnerElo loserElo winnerGameCount EloWon)]
        updateWhere [UserId ==. loserId] [UserElo =. (newElo loserElo winnerElo loserGameCount EloLost)]
      where
        loserElo = userElo loser
        winnerElo = userElo winner
    _ ->
      return ()
  where
    pType = playerType game winnerId
    -- ugly, but works
    loserFilter = case pType of
      Owner -> maybe ([]) (\userId -> [UserId ==. userId]) $ gameGuestId game
      Guest -> [UserId ==. gameOwnerId game]

-- validate the battlefield setup input and persist it in the database if it's correct
-- afterwards, if both players did already setup, broadcast each of them the view of the battlefield via websockets
setupBattlefield :: Entity User -> Entity Game -> SetupInputData -> Handler Value
setupBattlefield (Entity userId _) (Entity gameId game) (SetupInputData placements) = do
  requireGameState game requiredStateFunc
  case validateBattlefieldInput placements of
    Left err -> sendStatusJSON status400 $ object ["ok" .= False, "error" .= err]
    Right _ -> do
      games :: [Entity Game] <-
        runDB $
          ESQ.rawSql
            ("UPDATE game SET ship_placements = " ++ (fromString $ jsonSet) ++ ", state = " ++ newStateSQL ++ " WHERE id = ? RETURNING ??")
            [ toPersistValue $ fromSqlKey $ gameId
            ]
      let [Entity _ newGame] = games
      let newState = gameState newGame
      gameStateUpdateWriter gameId $ GameStateUpdatedEvent $ gameState newGame
      -- gamelog is streamed to the players -> they see when the other person finished their setup
      newGameLog gameId (Just pType) "SETUP DONE"
      case newState of
        -- this means both players have setup their battlefield, let's send them their view of the battlefield
        -- which, in the beginning, just includes their ship positions
        OwnerTurn -> do
          gameStateUpdateWriter gameId $
            GameBattlefieldUpdatedEvent Owner $ battlefieldStateView Owner battlefield shipPlacements
          gameStateUpdateWriter gameId $
            GameBattlefieldUpdatedEvent Guest $ battlefieldStateView Guest battlefield shipPlacements
          where
            JSONB battlefield = gameBattlefield newGame
            JSONB shipPlacements = gameShipPlacements newGame
        _ -> pure ()
      sendStatusJSON status200 $ object ["ok" .= True]
      where
        -- so, Setup True False means the owner finished the setup, while the guest did not
        -- we do the update in SQL so we don't have a race condition, since both players "could" submit their
        -- battlefield at the same time
        newStateSQL = case pType of
          Owner -> "CASE WHEN state = 'Setup False False' THEN 'Setup True False' ELSE 'OwnerTurn' END"
          Guest -> "CASE WHEN state = 'Setup False False' THEN 'Setup False True' ELSE 'OwnerTurn' END"
        jsonSet = generateJSONBSetForShipPlacement "ship_placements" pType placements
  where
    pType = playerType game userId

    -- make sure the player is allowed to setup the battlefield, he can't setup twice!
    requiredStateFunc :: (GameState -> Bool) = case pType of
      Owner -> (\state -> state == Setup False True || state == Setup False False)
      Guest -> (\state -> state == Setup True False || state == Setup False False)

-- a player clicked on a cell, let's validate it and persist it
registerHit :: Entity User -> Entity Game -> HitInputData -> Handler Value
registerHit userE@(Entity userId _) (Entity gameId game) (HitInputData row col) = do
  -- make sure it's his turn
  requireGameState game requiredStateFunc
  case validateHit game pType row col of
    -- a hit can fail, for example when it's out of bounds or the cell was already hit at an earlier turn
    Left err -> sendStatusJSON status400 $ object ["ok" .= False, "error" .= err]
    -- if its a valid hit, the function also returns if we hit a enemy ship and if all enemy ships are sunken now
    Right (didHit, allShipsSunk) -> do
      let newState = if allShipsSunk then "'Finished'" else "CASE WHEN state = 'OwnerTurn' THEN 'GuestTurn' ELSE 'OwnerTurn' END"
      games :: [Entity Game] <-
        runDB $
          ESQ.rawSql
            ("UPDATE game SET battlefield = jsonb_set(battlefield," ++ fromString path ++ ", 'true'), state = " ++ newState ++ " WHERE id = ? RETURNING ??")
            [ toPersistValue $ fromSqlKey $ gameId
            ]
      -- also ugly, but we now what we want here!
      let [newGameE@(Entity _ newGame)] = games
      let JSONB battlefield = gameBattlefield newGame
      let JSONB shipPlacements = gameShipPlacements newGame

      -- we're in a new Game state and the battlefield views for each client are now different, let's update the views of each one
      gameStateUpdateWriter gameId $ GameStateUpdatedEvent $ gameState newGame
      gameStateUpdateWriter gameId $ GameBattlefieldUpdatedEvent Owner $ battlefieldStateView Owner battlefield shipPlacements
      gameStateUpdateWriter gameId $ GameBattlefieldUpdatedEvent Guest $ battlefieldStateView Guest battlefield shipPlacements

      let hitMsg = if didHit then "HIT!" else "MISS!"
      newGameLog gameId (Just pType) ("Target: " ++ (fromString $ rowColToText (row, col)) ++ " - " ++ hitMsg)

      -- if all enemy ships are sunken, let's finish the game
      bool (return ()) (finishGame newGameE userE) allShipsSunk
      sendStatusJSON status200 $ object ["ok" .= True]
  where
    pType = playerType game userId
    pIndex = case pType of
      Owner -> "0"
      Guest -> "1"
    requiredStateFunc :: (GameState -> Bool) = case pType of
      Owner -> \state -> state == OwnerTurn
      Guest -> \state -> state == GuestTurn
    path = "'{" ++ (show row) ++ "," ++ (show col) ++ "," ++ pIndex ++ "}'"

-- we have a single entry for all user inputs regarding game operations
-- so two payloads can come in: Setup and Hit
postGameStateR :: Key Game -> Handler Value
postGameStateR gameId = do
  user <- requireAuth
  game <- requireGame gameId
  body :: Inputs <- requireCheckJsonBody
  case body of
    SetupInput dat -> setupBattlefield user game dat
    HitInput dat -> registerHit user game dat

-- this is requested by the VueJS part when the game site initially loads
-- it returns necessary info for the frontend, like the player type and the current state + battlefield view
gameView :: Key User -> Entity Game -> Handler Value
gameView userId (Entity gameId game) = do
  logs <- runDB $ selectList [GameLogGameId ==. gameId] []
  return $
    object
      [ "state" .= gameState game,
        "pType" .= pType,
        "stateView" .= battlefieldStateView pType battlefield shipPlacements,
        "logs" .= logs
      ]
  where
    pType = playerType game userId
    JSONB battlefield = gameBattlefield game
    JSONB shipPlacements = gameShipPlacements game

-- Can return two types of responses based on the request type:
-- JSON: Return the current game view
-- WebSockets: Supply a websocket connection which broadcasts state and battlefield updates to all clients
getGameStateR :: Key Game -> Handler Value
getGameStateR gameId = do
  gameE@(Entity _ game) <- requireGame gameId
  Entity userId _ <- requireAuth
  webSockets $ gameStateUpdateListener gameId $ playerType game userId
  gameView userId gameE
