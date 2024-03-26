module Ui.Logic (transformGame) where

import Game
  ( Game (gameState),
    State (GameOver, Running),
    initialGame,
  )
import Graphics.Gloss.Interface.Pure.Game
  ( Event (EventKey),
    Key (MouseButton),
    KeyState (Up),
    MouseButton (LeftButton),
  )
import Logic (playerTurn)
import Ui.Constants
  ( cellHeight,
    cellWidth,
    screenHeight,
    screenWidth,
  )

mousePosAsCellCoord :: (Float, Float) -> (Int, Int)
mousePosAsCellCoord (x, y) =
  ( floor ((y + (fromIntegral screenHeight * 0.5)) / cellHeight),
    floor ((x + (fromIntegral screenWidth * 0.5)) / cellWidth)
  )

transformGame :: Event -> Game -> Game
transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game =
  case gameState game of
    Running -> playerTurn game (mousePosAsCellCoord mousePos)
    GameOver _ -> initialGame
transformGame _ game = game
