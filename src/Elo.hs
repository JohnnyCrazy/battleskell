{-# LANGUAGE MultiWayIf #-}

module Elo where

-- elo implementation https://de.wikipedia.org/wiki/Elo-Zahl

data EloResult = EloWon | EloLost

expectedWinValue :: Int -> Int -> Float
expectedWinValue eloA eloB = 1 / (1 + 10 ** ((fromIntegral eloB - fromIntegral eloA) / 400))

newElo :: Int -> Int -> Int -> EloResult -> Int
newElo eloA eloB gameCount result =
  eloA + (round (k * (resultInt - expectedWinValue eloA eloB)))
  where
    resultInt = case result of
      EloWon -> 1
      EloLost -> 0
    k =
      if
          | eloA > 2400 -> 10
          | gameCount < 20 -> 40
          | otherwise -> 20
