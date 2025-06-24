kkkkkkmodule Main
where

import ChompPlusPlus
import Control.Monad (when)

curPlayer :: GameState -> String
curPlayer (_,_,_,_,player,_,_) = case player of
  P1 -> "Player 1"
  P2 -> "Player 2"

-- below we define a game for your testing purpose, feel free to change it
-- our testing and marking will only import your implementations from ChompPlusPlus.hs
gb :: GameBoard
gb = [ [Antidote 4, Empty, Coins 20, Coins 30, Antidote 1],
       [Poison 10, Antidote 6, Empty, Poison 2, Coins 5],
       [Empty, Empty, Poison 3, Antidote 2, Antidote 6],
       [Poison 4, Empty, Empty, Poison 3, Coins 10]
     ]

game :: GameState
game = (4,5,6,gb,P1,Just (PS_ac 0 0),Just (PS_ac 0 0))

main :: IO ()
main = do
  loop game

loop :: GameState -> IO ()
loop gs = do
  when (not (isGameEnd gs)) $ do
    mapM_ putStrLn (displayGame gs)
    putStrLn ("Now is " ++ curPlayer gs ++ "'s turn. Enter pointed cell by row number, followed by space and then column number.")
    input <- getLine
    let [r,c] = map read (words input) :: [Int]
    when (not (isLegalMove gs r c)) $ do
      putStrLn "*** This move is not legal. Please re-enter your move. ***"
      loop gs
    when (isLegalMove gs r c) $ do
      let new_game = makeMove gs r c
      loop new_game
  when (isGameEnd gs) $ do
    mapM_ putStrLn (displayGame gs)
    putStrLn $ endGameMsg gs