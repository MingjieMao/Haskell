module ChompPlusPlus
where

type NumInitRows = Int
type NumInitCols = Int
type MaxChomp = Int

data CellContent = Coins Int | Poison Int | Antidote Int | Empty deriving (Show,Eq)
type GameBoard = [[CellContent]]

data Player = P1 | P2 deriving (Show,Eq)

data PlayerState = PS_acb Int Int Int deriving (Show,Eq)

type GameState = (NumInitRows, NumInitCols, MaxChomp, GameBoard,
                    Player, Maybe PlayerState, Maybe PlayerState)
-- Player specifies which player makes the next move
-- the last two Maybe PlayerState refers to state of P1 and P2 respectively

-- Task 1: Check if game has ended
isGameEnd :: GameState -> Bool
isGameEnd (_, _, _, gameBoard, _, p1State, p2State) =
  all null gameBoard || p1State == Nothing || p2State == Nothing

-- Task 2: Display the game board
-- | Pads an integer to ensure consistent cell width
pad :: Int -> String
pad n = if n < 10 then show n ++ " " else show n

cellToString :: CellContent -> String
cellToString Empty = "    |"
cellToString (Coins n) = "C " ++ pad n ++ "|"
cellToString (Poison n) = "P " ++ pad n ++ "|"
cellToString (Antidote n) = "A " ++ pad n ++ "|"
  where pad x = if x < 10 then show x ++ " " else show x

rowToString :: [CellContent] -> Int -> Int -> String
rowToString cells r c =
  show r ++ "  |" ++ concatMap cellToString cells ++ replicate ((c - length cells) * 5) 'X'

displayGame :: GameState -> [String]
displayGame gs@(numRows, numCols, maxChomp, board, _, p1, p2) =
  ["    " ++ concatMap (\i -> "  " ++ show i ++ "  ") [0..numCols - 1], ""] ++
  [rowToString (board !! r) r numCols | r <- [0..numRows - 1]] ++
  [case p1 of
     Nothing -> "Player 1 dies."
     Just (PS_acb a b c) -> "Player 1's state: " ++ show a ++ " antidotes, " ++ show b ++ " coins, " ++ show c ++ " bonus." ] ++
  [case p2 of
     Nothing -> "Player 2 dies."
     Just (PS_acb a b c) -> "Player 2's state: " ++ show a ++ " antidotes, " ++ show b ++ " coins, " ++ show c ++ " bonus." ] ++
  (if isGameEnd gs then [] else ["Maximum number of cells can be chomped in one move is " ++ show maxChomp ++ "."])

-- Task 3: Check legal move
isLegalMove :: GameState -> Int -> Int -> Bool
isLegalMove (numRows, numCols, maxChomp, board, _, _, _) r c =
  r >= 0 && r < numRows && c >= 0 && c < numCols &&
  r < length board && c < length (board !! r) &&
  sum [length (drop c row) | row <- take (r+1) board] <= maxChomp

-- Task 4: Make move
makeMove :: GameState -> Int -> Int -> GameState
makeMove (numRows, numCols, maxChomp, board, player, p1, p2) r c =
  (numRows, numCols, maxChomp, newBoard, nextPlayer, newP1, newP2)
  where
    chomped = concatMap (drop c) (take (r+1) board)
    newBoard = [if i <= r then take c row else row | (i, row) <- zip [0..] board]
    nextPlayer = if player == P1 then P2 else P1
    updateState (Just (PS_acb a b bonus)) =
      let coins = sum [n | Coins n <- chomped]
          poison = sum [n | Poison n <- chomped]
          antidote = sum [n | Antidote n <- chomped]
          newAntidote = a - poison + antidote
          newCoins = b + coins
      in if poison > a then Nothing else Just (PS_acb newAntidote newCoins bonus)
    updateState Nothing = Nothing
    newP1 = if player == P1 then updateState p1 else p1
    newP2 = if player == P2 then updateState p2 else p2

-- Task 5: End game message
endGameMsg :: GameState -> String
endGameMsg (_, _, _, _, _, Nothing, Just _) = "Player 2 wins because Player 1 dies."
endGameMsg (_, _, _, _, _, Just _, Nothing) = "Player 1 wins because Player 2 dies."
endGameMsg (_, _, _, _, _, Just (PS_acb _ c1 _), Just (PS_acb _ c2 _))
  | c1 > c2 = "Player 1 wins because Player 1 has " ++ show c1 ++ " coins, which are more than " ++ show c2 ++ " coins of Player 2."
  | c2 > c1 = "Player 2 wins because Player 2 has " ++ show c2 ++ " coins, which are more than " ++ show c1 ++ " coins of Player 1."
  | otherwise = "The game ends up with a draw because both players have " ++ show c1 ++ " coins."