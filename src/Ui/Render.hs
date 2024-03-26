module Ui.Render (gameAsPicture) where

import Constants (rowColumnSize)
import Data.Array (assocs)
import Game
  ( Board,
    Cell,
    Game (gameBoard, gameState),
    Player (PlayerO, PlayerX),
    State (GameOver, Running),
  )
import Graphics.Gloss
  ( Color,
    Picture,
    color,
    line,
    pictures,
    rectangleSolid,
    rotate,
    thickCircle,
    translate,
  )
import Ui.Constants
  ( boardGridColor,
    cellHeight,
    cellWidth,
    playerOColor,
    playerXColor,
    screenHeight,
    screenWidth,
    tieColor,
  )

alignPictureToCell :: (Integral a, Integral b) => Picture -> (a, b) -> Picture
alignPictureToCell picture (row, column) = translate x y picture
  where
    x = fromIntegral column * cellWidth + cellWidth * 0.5
    y = fromIntegral row * cellHeight + cellHeight * 0.5

getPicturesOfCells :: Board -> Cell -> Picture -> Picture
getPicturesOfCells board cell cellPicture =
  pictures
    ( map
        (alignPictureToCell cellPicture . fst)
        ( filter
            (\(_, e) -> e == cell)
            ( assocs board
            )
        )
    )

getXCellPicture :: Picture
getXCellPicture =
  pictures
    [ rotate 45.0 (rectangleSolid side 10.0),
      rotate (-45.0) (rectangleSolid side 10.0)
    ]
  where
    side = min cellWidth cellHeight * 0.75

getPicturesOfXCells :: Board -> Picture
getPicturesOfXCells board = getPicturesOfCells board (Just PlayerX) getXCellPicture

getOCellPicture :: Picture
getOCellPicture = thickCircle radius 10.0
  where
    radius = min cellWidth cellHeight * 0.25

getPicturesOfOCells :: Board -> Picture
getPicturesOfOCells board = getPicturesOfCells board (Just PlayerO) getOCellPicture

getBoardGridPicture :: Picture
getBoardGridPicture =
  pictures
    ( concatMap
        ( \i ->
            [ line
                [ (i * cellWidth, 0.0),
                  (i * cellWidth, fromIntegral screenHeight)
                ],
              line
                [ (0.0, i * cellHeight),
                  (fromIntegral screenWidth, i * cellHeight)
                ]
            ]
        )
        [0.0 .. fromIntegral rowColumnSize]
    )

getRunningGamePicture :: Board -> Picture
getRunningGamePicture board =
  pictures
    [ color playerXColor (getPicturesOfXCells board),
      color playerOColor (getPicturesOfOCells board),
      color boardGridColor getBoardGridPicture
    ]

getBoardPicture :: Board -> Picture
getBoardPicture board =
  pictures
    [ getPicturesOfXCells board,
      getPicturesOfOCells board,
      getBoardGridPicture
    ]

getOutcomeColor :: Maybe Player -> Color
getOutcomeColor (Just PlayerX) = playerXColor
getOutcomeColor (Just PlayerO) = playerOColor
getOutcomeColor Nothing = tieColor

getGameOverPicture :: Maybe Player -> Board -> Picture
getGameOverPicture winner board = color (getOutcomeColor winner) (getBoardPicture board)

gameAsPicture :: Game -> Picture
gameAsPicture game =
  translate
    (fromIntegral screenWidth * (-0.5))
    (fromIntegral screenHeight * (-0.5))
    frame
  where
    frame = case gameState game of
      Running -> getRunningGamePicture (gameBoard game)
      GameOver winner -> getGameOverPicture winner (gameBoard game)
