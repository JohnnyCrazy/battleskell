{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Game where

import Database.Esqueleto ((?.), (^.))
import qualified Database.Esqueleto as E
import Database.Persist.Postgresql
import GameLog
import GameStateUpdater
import Import
import Model.Game
import Settings.StaticFiles

joinGame :: Key Game -> Entity User -> Handler Bool
joinGame gameId (Entity userId _) = do
  now <- liftIO $ getCurrentTime
  modifiedRows <-
    -- the filter makes sure we're not joining our own game/the guest slot is still empty
    runDB $
      updateWhereCount
        [ GameId ==. gameId,
          GameGuestId ==. Nothing,
          GameOwnerId !=. userId
        ]
        [GameGuestId =. (Just userId), GameGuestLastPing =. (Just now), GameState =. Setup False False]
  case modifiedRows of
    0 -> return False
    _ -> do
      -- since we need a join, let's use esqueleto!
      games <- runDB $
        E.select $
          E.from $ \(game `E.InnerJoin` owner `E.InnerJoin` guest) -> do
            E.on $ game ^. GameOwnerId E.==. owner ^. UserId
            E.on $ game ^. GameGuestId E.==. guest ?. UserId
            E.where_ $ game ^. GameId E.==. E.val gameId
            return
              (game, owner, guest)
      case games of
        [(Entity _ game, Entity _ owner, Just (Entity _ guest))] -> do
          -- websocket message to the game itself: We're in a new state, let's setup websockets!
          gameStateUpdateWriter gameId $ GameStateUpdatedEvent $ gameState game

          -- websocket message to the lobby room: A game is now full so lets remove it from the lobby list
          let payload = LobbyRemovedEvent gameId
          updateChannel <- lobbyChannel <$> getYesod
          atomically $ writeTChan updateChannel payload

          -- We're also writing in the gamelog, so both parties know when the game started and who is part of the game
          newGameLog gameId Nothing ("Game started: " <> userUsername owner <> " vs. " <> userUsername guest)
          return True
        _ -> do
          return False

-- ingame view, vuejs takes over here
getGameR :: Key Game -> Handler Html
getGameR _ = do
  defaultLayout $
    $(widgetFile "game")

-- join an open game
-- if it fails, render the erros via errors widgetFile
-- if it succeeds, redirect to the game so the game can start!
postGameR :: Key Game -> Handler TypedContent
postGameR gameId = do
  userE <- requireAuth
  joined <- joinGame gameId userE
  case joined of
    -- TODO: Ugly but works?
    False -> do
      errorLayout <- defaultLayout $ do
        let errors :: [Text] =
              [ "Can't join your own game",
                "Can't join an already full game",
                "Can't join a non-existing game"
              ]
        $(widgetFile "error")
      pure (toTypedContent errorLayout)
    True -> redirect $ GameR gameId
