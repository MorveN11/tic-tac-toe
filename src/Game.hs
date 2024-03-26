module Game
  ( Board,
    Cell,
    Player (PlayerO, PlayerX),
    State (Running, GameOver),
    Game (Game, gameBoard, gamePlayer, gameState),
    initialGame,
  )
where

import Constants (rowColumnSize)
import Data.Array (Array, Ix (range), array)

data Player = PlayerX | PlayerO deriving (Eq, Show)

type Cell = Maybe Player

data State = Running | GameOver (Maybe Player) deriving (Eq, Show)

type Board = Array (Int, Int) Cell

data Game = Game
  { gameBoard :: Board,
    gamePlayer :: Player,
    gameState :: State
  }
  deriving (Eq, Show)

initialGame :: Game
initialGame =
  Game
    { gameBoard = array indexRange [(i, Nothing) | i <- range indexRange],
      gamePlayer = PlayerX,
      gameState = Running
    }
  where
    indexRange = ((0, 0), (rowColumnSize - 1, rowColumnSize - 1))
