{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Home where

import Database.Esqueleto ((^.))
import qualified Database.Esqueleto as E
import Import
import Model.Game

-- leadboard of all clients who have >1 game played
-- returns the user and the number of games he played
fetchLeaderboard :: Handler ([(Entity User, E.Value Int)])
fetchLeaderboard =
  runDB $
    E.select $
      E.from $ \(user `E.RightOuterJoin` game) -> do
        E.where_ $ (game ^. GameState E.==. E.val Finished)
        E.on $ (game ^. GameOwnerId E.==. user ^. UserId E.||. game ^. GameGuestId E.==. (E.just $ user ^. UserId))
        E.orderBy [E.desc $ user ^. UserElo]
        E.groupBy (user ^. UserId)
        return (user, E.countRows)

-- simple static yesod page with leaderboard
getHomeR :: Handler Html
getHomeR = do
  _ <- getMessage
  user <- getUser <$> maybeAuth
  leaderboard <- fetchLeaderboard
  defaultLayout $ do
    $(widgetFile "homepage")
