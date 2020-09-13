{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module WebSocketsExtension
  ( sendJSONData,
    receiveJSONData,
    receiveJSONDataWithPing,
    receiveJSONDataWithPing_,
    PingPongMsg (..),
  )
where

import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as DBLazy
import Import
import Network.WebSockets (Connection)
import Yesod.WebSockets (receiveData, sendTextData)

-- this whole class is just big wrapper around json messages received from websockets
-- Initially, I thought the client may also sends data via websockets so receiving custom data has to be supported.
-- In the end, websockets are just used from server to client with the exception of a simple "ping" -> "pong" exchange, initiated from the client
data PingPongMsg a
  = Ping
  | Pong
  | NotPing a
  deriving (Generic, Show, Typeable)

instance ToJSON a => ToJSON (PingPongMsg a) where
  toJSON (NotPing a) = toJSON a
  toJSON Ping = "ping"
  toJSON Pong = "pong"

instance FromJSON a => FromJSON (PingPongMsg a) where
  parseJSON (String "ping") = return Ping
  parseJSON (String "pong") = return Pong
  parseJSON a = do
    parsed <- parseJSON a
    return (NotPing parsed)

-- receive json data, making sure FromJSON is defined for the type
receiveJSONData :: forall (m :: * -> *) a. (MonadIO m, MonadReader Connection m, FromJSON a) => m (Maybe a)
receiveJSONData = do
  msg :: DBLazy.ByteString <- receiveData
  let decoded :: Maybe a = decode $ msg
  return decoded

-- receive json data, making sure FromJSON is defined for the type
-- but also respond to pings with a pong, so the connection stays alive
receiveJSONDataWithPing :: forall (m :: * -> *) a. (MonadIO m, MonadReader Connection m, FromJSON a) => m (Maybe (PingPongMsg a))
receiveJSONDataWithPing = do
  msg :: DBLazy.ByteString <- receiveData
  let decoded :: Maybe (PingPongMsg a) = decode $ msg
  case decoded of
    Just (NotPing a) -> return $ Just $ NotPing a
    Just Ping -> do
      sendJSONData "pong" (Pong :: PingPongMsg ())
      return $ Just Ping
    Just Pong -> return $ Just Pong
    _ -> return Nothing

-- receive any data and respond with ping, but ignore result
receiveJSONDataWithPing_ :: (MonadIO m, MonadReader Connection m) => m ()
receiveJSONDataWithPing_ = do
  _ :: Maybe (PingPongMsg Text) <- receiveJSONDataWithPing
  return ()

sendJSONData :: (ToJSON a, MonadIO m, MonadReader Connection m) => String -> a -> m ()
sendJSONData typeId val =
  sendTextData $
    encode $
      object
        [ "id" .= typeId,
          "val" .= val
        ]
