{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Games where

import Data.Aeson
import Database.Esqueleto ((^.))
import qualified Database.Esqueleto as E
import Import
import Model.Game
import Settings.StaticFiles
import WebSocketsExtension
import Yesod.WebSockets

-- a helper function which returns games and its owner based on a GameState
getGamesInState :: GameState -> Handler [(Entity Game, E.Value Text)]
getGamesInState state = runDB $
  E.select $
    E.from $ \(game `E.InnerJoin` user) -> do
      E.on $ game ^. GameOwnerId E.==. user ^. UserId
      E.where_ $ game ^. GameState E.==. E.val state
      return
        (game, user ^. UserUsername)

-- same as getGamesInState but returns the first, if existing
getGameInState :: GameId -> Handler (Maybe (Entity Game, E.Value Text))
getGameInState gameId = do
  games <- runDB $
    E.select $
      E.from $ \(game `E.InnerJoin` user) -> do
        E.on $ game ^. GameOwnerId E.==. user ^. UserId
        E.where_ $ game ^. GameId E.==. E.val gameId
        return
          (game, user ^. UserUsername)
  case games of
    xs : _ -> return (Just xs)
    _ -> return Nothing

-- our lobby websocket app
-- makes the lobby view real-time by supplying new/removed lobbies via websockets
updateLobbies :: TChan LobbyEvent -> WebSocketsT Handler ()
updateLobbies updateChannel = do
  -- duplicate the broadcast channel, so we can read from it
  myChannel <- atomically $ dupTChan updateChannel
  -- since we have two operations we need to listen on (sending/receving messages),
  -- let's run them concurrently.
  race_
    (forever $ receiveJSONDataWithPing_) -- we're not expecting any messages, so only respond to ping messages
    ( forever $ do
        -- use the channel to get new messages which need to be sent to the clients
        payload <- atomically $ readTChan myChannel
        case payload of
          LobbyAddedEvent (game, username) -> sendJSONData "lobby-added" (object ["username" .= username, "game" .= game])
          LobbyRemovedEvent gameId -> sendJSONData "lobby-removed" gameId
    )

-- this response handler can return 3 different results based on the request type:
-- HTML: Return a VueJS based widget file
-- JSON: Return a list of games which are in the lobby, including their owner username
-- WebSockets: Wait for new events (like lobby added/removed) and push them to the client
getGamesR :: Handler TypedContent
getGamesR = do
  -- make sure we're authed
  _ <- requireAuthId
  updateChannel <- lobbyChannel <$> getYesod
  -- This call actually short-circuits! The part after will not be called if the request was a websocket request
  webSockets $ updateLobbies updateChannel
  games <- getGamesInState Lobby
  selectRep $ do
    provideRep $
      defaultLayout $ do
        $(widgetFile "games")
    provideJson $
      map (\(g, username) -> object ["game" .= g, "username" .= username]) games

-- this handler creates a new game
-- it also broadcasts to all connected websockets, that a lobby is now available to join
postGamesR :: Handler TypedContent
postGamesR = do
  userId <- requireAuthId
  thisGame <- liftIO $ newEmptyGame userId
  gameId <- runDB $ insert $ thisGame
  mgameWithUser <- getGameInState gameId
  -- this part is a little bit ugly. We know that the game exists 100%, but the types don't!
  case mgameWithUser of
    Just gameWithUser -> do
      updateChannel <- lobbyChannel <$> getYesod
      atomically $ writeTChan updateChannel payload
      redirect $ GameR gameId
      where
        payload = LobbyAddedEvent gameWithUser
    _ -> redirect HomeR
