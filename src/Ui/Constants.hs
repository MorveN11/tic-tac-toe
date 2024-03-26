module Ui.Constants
  ( cellHeight,
    cellWidth,
    boardGridColor,
    playerOColor,
    playerXColor,
    screenHeight,
    screenWidth,
    tieColor,
    backgroundColor,
    fps,
    window,
  )
where

import Constants (gameTitle, rowColumnSize)
import Graphics.Gloss (Color, Display (InWindow), makeColorI)

screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 480

gamePosition :: (Int, Int)
gamePosition = (100, 100)

window :: Display
window = InWindow gameTitle (screenWidth, screenHeight) gamePosition

fps :: Int
fps = 30

cellWidth :: Float
cellWidth = fromIntegral screenWidth / fromIntegral rowColumnSize

cellHeight :: Float
cellHeight = fromIntegral screenHeight / fromIntegral rowColumnSize

playerXColor :: Color
playerXColor = makeColorI 0 128 0 255

playerOColor :: Color
playerOColor = makeColorI 253 91 42 255

backgroundColor :: Color
backgroundColor = makeColorI 255 255 255 255

boardGridColor :: Color
boardGridColor = makeColorI 0 0 0 255

tieColor :: Color
tieColor = makeColorI 130 135 141 255
