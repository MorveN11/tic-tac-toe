module Console.Render (gameAsText) where

import Constants (gameTitle, rowColumnSize)
import Data.Array ((!))
import Game (Board, Game (gameBoard, gamePlayer, gameState), Player (PlayerO, PlayerX), State (GameOver, Running))

cellAsText :: Board -> (Int, Int) -> String
cellAsText board (y, x) =
  case board ! (y, x) of
    Just PlayerX -> "X"
    Just PlayerO -> "O"
    Nothing -> "_"

rowAsText :: Board -> Int -> String
rowAsText board y =
  unwords ([cellAsText board (y, x) | x <- [0 .. rowColumnSize - 1]])

boardAsText :: Board -> String
boardAsText board =
  unlines ([rowAsText board y | y <- [0 .. rowColumnSize - 1]])

boardAsRunningText :: Player -> Board -> String
boardAsRunningText PlayerX board = gameTitle ++ "\nPlayer X's turn\n" ++ boardAsText board
boardAsRunningText PlayerO board = gameTitle ++ "\nPlayer O's turn\n" ++ boardAsText board

boardAsGameOverText :: Maybe Player -> Board -> String
boardAsGameOverText (Just PlayerO) _ = "Player O wins!"
boardAsGameOverText (Just PlayerX) _ = "Player X wins!"
boardAsGameOverText Nothing _ = "It's a draw!"

gameAsText :: Game -> String
gameAsText game =
  text
  where
    text = case gameState game of
      Running -> boardAsRunningText (gamePlayer game) (gameBoard game)
      GameOver winner -> boardAsGameOverText winner (gameBoard game)
