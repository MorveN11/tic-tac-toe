import Data.Array (listArray)
import Data.Maybe (isNothing)
import Game (Board, Game (Game, gameBoard), Player (PlayerO, PlayerX), State (GameOver))
import Logic (winner)
import Test.QuickCheck
  ( Property,
    forAll,
    quickCheck,
  )

winningBoardX :: Board
winningBoardX = listArray ((0, 0), (2, 2)) (replicate 3 (Just PlayerX) ++ replicate 6 Nothing)

winningGameX :: Game
winningGameX = Game winningBoardX PlayerX (GameOver (Just PlayerX))

prop_PlayerXWins :: Property
prop_PlayerXWins =
  forAll
    (return winningGameX)
    ( \game ->
        winner (gameBoard game) == Just PlayerX
    )

winningBoardO :: Board
winningBoardO = listArray ((0, 0), (2, 2)) (replicate 3 (Just PlayerO) ++ replicate 6 Nothing)

winningGameO :: Game
winningGameO = Game winningBoardO PlayerO (GameOver (Just PlayerO))

prop_PlayerOWins :: Property
prop_PlayerOWins =
  forAll
    (return winningGameO)
    ( \game ->
        winner (gameBoard game) == Just PlayerO
    )

tieBoard :: Board
tieBoard =
  listArray
    ((0, 0), (2, 2))
    [ Just PlayerX,
      Just PlayerO,
      Just PlayerX,
      Just PlayerX,
      Just PlayerX,
      Just PlayerO,
      Just PlayerO,
      Just PlayerX,
      Just PlayerO
    ]

tieGame :: Game
tieGame = Game tieBoard PlayerX (GameOver Nothing)

prop_GameIsTie :: Property
prop_GameIsTie =
  forAll
    (return tieGame)
    ( isNothing . winner . gameBoard
    )

main :: IO ()
main = do
  quickCheck prop_PlayerXWins
  quickCheck prop_PlayerOWins
  quickCheck prop_GameIsTie
  putStrLn "All tests passed!"
