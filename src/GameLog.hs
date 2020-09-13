module GameLog where

import GameStateUpdater
import Import
import Model.Game

-- simply insert in db and publish a new event to the game participants
newGameLog :: Key Game -> Maybe PlayerType -> Text -> Handler ()
newGameLog gameId pType msg = do
  now <- liftIO $ getCurrentTime
  gameLog <- runDB $ insertEntity $ GameLog gameId pType msg now
  gameStateUpdateWriter gameId $ GameLogUpdatedEvent gameLog
