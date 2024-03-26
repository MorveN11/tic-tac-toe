module Logic (playerTurn, winner) where

import Constants (rowColumnSize)
import Data.Array (Ix (inRange), elems, (!), (//))
import Data.Foldable (asum)
import Data.Maybe (isNothing)
import Game
  ( Board,
    Cell,
    Game (gameBoard, gamePlayer, gameState),
    Player (PlayerO, PlayerX),
    State (GameOver),
  )

countCells :: Cell -> Board -> Int
countCells cell = length . filter (cell ==) . elems

allSame :: [Cell] -> Maybe Player
allSame (cell@(Just player) : cells) | all (== cell) cells = Just player
allSame _ = Nothing

getRows :: Board -> [[Cell]]
getRows board = [[board ! (i, j) | i <- [0 .. rowColumnSize - 1]] | j <- [0 .. rowColumnSize - 1]]

getCols :: Board -> [[Cell]]
getCols board = [[board ! (j, i) | i <- [0 .. rowColumnSize - 1]] | j <- [0 .. rowColumnSize - 1]]

getDiags :: Board -> [[Cell]]
getDiags board =
  [ [board ! (i, i) | i <- [0 .. rowColumnSize - 1]],
    [board ! (i, j) | i <- [0 .. rowColumnSize - 1], let j = rowColumnSize - 1 - i]
  ]

winner :: Board -> Maybe Player
winner board = asum (map allSame (getRows board ++ getCols board ++ getDiags board))

checkGameOver :: Game -> Game
checkGameOver game
  | Just player <- winner board =
      game {gameState = GameOver (Just player)}
  | countCells Nothing board == 0 =
      game {gameState = GameOver Nothing}
  | otherwise = game
  where
    board = gameBoard game

switchPlayer :: Game -> Game
switchPlayer game =
  case gamePlayer game of
    PlayerX -> game {gamePlayer = PlayerO}
    PlayerO -> game {gamePlayer = PlayerX}

isCoordCorrect :: (Int, Int) -> Bool
isCoordCorrect = inRange ((0, 0), (rowColumnSize - 1, rowColumnSize - 1))

playerTurn :: Game -> (Int, Int) -> Game
playerTurn game cellCoord
  | isCoordCorrect cellCoord && isNothing (board ! cellCoord) =
      checkGameOver
        ( switchPlayer
            ( game {gameBoard = board // [(cellCoord, Just player)]}
            )
        )
  | otherwise = game
  where
    board = gameBoard game
    player = gamePlayer game
