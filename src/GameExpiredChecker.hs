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

module GameExpiredChecker where

import Data.Time (addUTCTime, secondsToNominalDiffTime)
import Import
import Model.Game

-- check if a game is expired and cancel it
checkOpenGames :: MonadIO m => SqlPersistT m ()
checkOpenGames = do
  now <- liftIO $ getCurrentTime
  let expiredDate = addUTCTime maxLastPing now
  -- TODO: We would like to use newLog or gameStateUpdateWriter here,
  -- but I didn't find a way to get Handler monad in here
  updateWhere
    ( [GameOwnerLastPing <=. expiredDate, GameState !=. Cancelled, GameState !=. Finished]
        ||. [GameGuestId !=. Nothing, GameGuestLastPing <=. Just expiredDate, GameState !=. Cancelled, GameState !=. Finished]
    )
    [GameState =. Cancelled, GameCancelReason =. Just "One of the participants left the game"]
  where
    maxLastPing = secondsToNominalDiffTime (-60)
