module Console.TicTacToe (playGame) where

import Console.Logic (transformGame)
import Console.Render (gameAsText)
import Game (Game (gameState), State (GameOver, Running), initialGame)

play :: Game -> (Game -> String) -> (String -> Game -> Game) -> IO ()
play game render eventHandler = do
  putStrLn (render game)
  case gameState game of
    Running -> putStrLn "Enter move:"
    GameOver _ -> putStrLn "Press Enter to restart"
  move <- getLine
  let newGame = eventHandler move game
  play newGame render eventHandler

playGame :: IO ()
playGame = play initialGame gameAsText transformGame
