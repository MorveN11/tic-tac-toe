module Console.Logic (transformGame) where

import Game (Game (gameState), State (GameOver, Running), initialGame)
import Logic (playerTurn)

stringAsCellCord :: String -> (Int, Int)
stringAsCellCord pos = (read [head pos], read [pos !! 1])

transformGame :: String -> Game -> Game
transformGame pos game =
  case gameState game of
    Running -> playerTurn game (stringAsCellCord pos)
    GameOver _ -> initialGame
