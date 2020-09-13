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

module GameStateUpdater where

import qualified Data.Map as Map
import Import
import Model.Game
import WebSocketsExtension
import Yesod.WebSockets

gameChannel :: Key Game -> Handler (TChan GameEvent)
gameChannel gameId = do
  allChannelsT <- gameChannels <$> getYesod
  atomically $ do
    allChannels <- readTVar allChannelsT
    channel <- pure $ Map.lookup gameId allChannels
    case channel of
      Just chan -> return chan
      Nothing -> do
        chan :: TChan GameEvent <- newTChan
        newMap <- pure $ Map.insert gameId (chan) allChannels
        writeTVar allChannelsT newMap
        return chan

gameStateUpdateListener :: Key Game -> PlayerType -> WebSocketsT Handler ()
gameStateUpdateListener gameId pType = do
  updateChannel <- lift $ gameChannel gameId
  myChannel <- atomically $ dupTChan updateChannel
  race_
    ( forever $ do
        -- in the lobby websocket we could ignore the ping, we don't care how much he pings us ans when was the last ping
        -- in the game, we want to make sure players are still there by checking when they last pinged us.
        -- This allows to reconnect easily, but makes sure there are no stalling games
        -- Thus, we need to track when we received pings and persist them in the DB.
        res :: Maybe (PingPongMsg ()) <- receiveJSONDataWithPing
        case res of
          Just Ping -> do
            now <- liftIO $ getCurrentTime
            lift $ runDB $ updateWhere [GameId ==. gameId] (updateSet now)
            where
              updateSet date = case pType of
                Guest -> [GameGuestLastPing =. Just date]
                Owner -> [GameOwnerLastPing =. date]
          _ -> return ()
    )
    ( forever $ do
        payload <- atomically $ readTChan myChannel
        case payload of
          GameStateUpdatedEvent newState -> sendJSONData "state-update" newState
          -- Since each player has a different view on the battlefield, we need to make sure we send the correct battlefield
          -- to the correct player
          GameBattlefieldUpdatedEvent targetPType newBattlefieldStateView
            | targetPType == pType ->
              sendJSONData "battlefield-update" newBattlefieldStateView
            | otherwise -> return ()
          GameLogUpdatedEvent newLog -> sendJSONData "log-added" newLog
    )

gameStateUpdateWriter :: Key Game -> GameEvent -> Handler ()
gameStateUpdateWriter gameId event = do
  chan <- gameChannel gameId
  atomically $ writeTChan chan event
