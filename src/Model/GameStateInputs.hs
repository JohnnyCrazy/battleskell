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

module Model.GameStateInputs where

import Data.Aeson (withObject)
import qualified Data.HashMap.Lazy as HML (lookup)
import Import
import Model.Game
import Prelude (MonadFail (fail))

data SetupInputData = SetupInputData [ShipPlacement]
  deriving (Generic, Show, Eq)

data HitInputData = HitInputData Int Int
  deriving (Generic, Show, Eq)

data Inputs = SetupInput SetupInputData | HitInput HitInputData

-- some FromJSON Magic for parsing the two different inputs that can be received
-- I thought I need to handle more than 2 inputs, thus this big JSON handling
-- It probably would be easier just to make two different endpoints for /setup and /hit
instance FromJSON Inputs where
  parseJSON = withObject "Input" $ \o -> do
    let val = HML.lookup "val" o
    case HML.lookup "id" o of
      Just (String "setup") -> setupInput val
      Just (String "hit") -> hitInput val
      _ -> fail $ "Unkown ID received: " ++ show (HML.lookup "type" o)
    where
      setupInput (Just d) = SetupInput <$> (SetupInputData <$> parseJSON d)
      setupInput _ = incorrectData

      hitInput (Just (Object o)) = HitInput <$> (HitInputData <$> o .: "row" <*> o .: "col")
      hitInput _ = incorrectData

      incorrectData = fail "Incorrect data received."
