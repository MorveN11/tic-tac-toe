module Ui.TicTacToe (playGame) where

import Game (initialGame)
import Graphics.Gloss (play)
import Ui.Constants (backgroundColor, fps, window)
import Ui.Logic (transformGame)
import Ui.Render (gameAsPicture)

playGame :: IO ()
playGame = play window backgroundColor fps initialGame gameAsPicture transformGame (const id)
