{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Model where

import ClassyPrelude.Yesod
import Database.Esqueleto.PostgreSQL.JSON
import Database.Persist.Quasi
import Model.Game

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/models.persistentmodels")

-- at the start, no hits have been made, so fill everything with (False, False)
emptyBattlefield :: Battlefield
emptyBattlefield = fromList [fromList ([(False, False) | _ <- [0 :: Int .. 9]]) | _ <- [0 :: Int .. 9]]

newEmptyGame :: Key User -> IO Game
newEmptyGame userId = do
  now <- liftIO $ getCurrentTime
  return $ Game Lobby userId now Nothing Nothing Nothing (JSONB emptyBattlefield) (JSONB ([], []))
