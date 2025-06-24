module ChompPlusPlus
where

type NumInitRows = Int
type NumInitCols = Int
type MaxChomp = Int

data CellContent = Coins Int | Poison Int | Antidote Int | Empty deriving (Show,Eq)
type GameBoard = [[CellContent]]

data Player = P1 | P2 deriving (Show,Eq)

data PlayerState = PS_ac Int Int deriving (Show,Eq)

type GameState = (NumInitRows, NumInitCols, MaxChomp, GameBoard,
                    Player, Maybe PlayerState, Maybe PlayerState)
-- Player specifies which player makes the next move
-- the last two Maybe PlayerState refers to state of P1 and P2 respectively

-- | Calculates the total number of coins from a list of CellContent.
-- >>> totalCoins [Coins 3, Empty, Coins 6, Poison 2, Coins 1]
-- 10
totalCoins :: [CellContent] -> Int
totalCoins = sum.map getCoins
  where
    getCoins (Coins x) = x
    getCoins _         = 0

-- | Calculates the total number of antidotes from a list of CellContent.
-- >>> totalAntidote [Antidote 2, Coins 3, Empty, Antidote 5]
-- 7
totalPoison :: [CellContent] -> Int
totalPoison = sum.map getPoison
  where
    getPoison (Poison x) = x
    getPoison _          = 0

-- | Calculates the total number of poison doses from a list of CellContent.
-- >>> totalPoison [Coins 3, Poison 5, Empty, Poison 4, Antidote 1]
-- 9
totalAntidote :: [CellContent] -> Int
totalAntidote = sum.map getAntidote
  where
    getAntidote (Antidote x) = x
    getAntidote _            = 0

-- | Adjusts the player's state based on a list of chomped cells.
-- >>> adjustPlayerState [Antidote 2, Poison 1, Poison 2] (PS_ac 1 3)
-- Nothing
-- >>> adjustPlayerState [Coins 4, Poison 2, Empty, Antidote 6, Poison 1, Coins 5] (PS_ac 4 10)
-- Just (PS_ac 7 19)
adjustPlayerState :: [CellContent] -> PlayerState -> Maybe PlayerState
adjustPlayerState chompedCells (PS_ac antidote coins)
  | newPoison > antidote = Nothing                            
  | otherwise            = Just (PS_ac newAntidote newCoins)  
  where
    newCoins    = coins + totalCoins chompedCells
    newPoison   = totalPoison chompedCells
    newAntidote = antidote - newPoison + totalAntidote chompedCells

-- Task 1
-- The game reaches an end if all cells in the gameboard are chomped or one of the players dies.
isGameEnd :: GameState -> Bool
isGameEnd (_, _, _, gameboard, _, p1State, p2State) = 
  all null gameboard || p1State == Nothing || p2State == Nothing

-- Task 2a
-- | Convert the CellContent to String.
-- >>> cellToString (Coins 7)
-- "C 7 |"
-- >>> cellToString (Coins 67)
-- "C 67|"
cellToString :: CellContent -> String
cellToString Empty        = "    |"
cellToString (Coins n)    = "C " ++ (if n < 10 then show n ++ " " else show n) ++ "|"
cellToString (Poison n)   = "P " ++ (if n < 10 then show n ++ " " else show n) ++ "|"
cellToString (Antidote n) = "A " ++ (if n < 10 then show n ++ " " else show n) ++ "|"
  
-- Task 2b
-- | Convert the row to String.
-- >>> rowToString [] 3 4 
-- "3  |XXXXXXXXXXXXXXXXXXXX"
-- >>> rowToString [Coins 3, Antidote 12, Poison 23] 7 5 
-- "7  |C 3 |A 12|P 23|XXXXXXXXXX"
-- >>> rowToString [Coins 3, Antidote 12, Poison 23, Empty, Coins 99] 7 5 
-- "7  |C 3 |A 12|P 23|    |C 99|"
rowToString :: [CellContent] -> Int -> Int -> String
rowToString cc r c = 
  show r ++ "  |" ++ concatMap cellToString cc ++ replicate ((c - length cc) * 5) 'X'

-- Task 2
-- | Display the game state.
-- >>> (5,7,5,[[],[Coins 12,Coins 4],[Empty,Poison 3,Antidote 11],[Antidote 2,Poison 10,Empty],
-- >>> [Empty,Empty,Coins 88,Empty]],P1,Just (PS_ac 11 3),Just (PS_ac 2 12))
-- ["      0    1    2    3    4    5    6","","0  |XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX",
-- "1  |C 12|C 4 |XXXXXXXXXXXXXXXXXXXXXXXXX","2  |    |P 3 |A 11|XXXXXXXXXXXXXXXXXXXX",
-- "3  |A 2 |P 10|    |XXXXXXXXXXXXXXXXXXXX","4  |    |    |C 88|    |XXXXXXXXXXXXXXX",
-- "Player 1's state: 11 antidotes, 3 coins.","Player 2's state: 2 antidotes, 12 coins.",
-- "Maximum number of cells can be chomped in one move is 5."]
displayGame :: GameState -> [String]
displayGame gs@(numInitRows, numInitCols, maxChomp, gameBoard, _, p1State, p2State) =
  ["    " ++ concatMap (("  " ++).(++ "  ").show) [0 .. numInitCols - 1]] ++
  [""] ++
  [rowToString (gameBoard !! r) r numInitCols | r <- [0 .. numInitRows - 1]] ++
  [case p1State of
    Nothing          -> "Player 1 dies."
    Just (PS_ac a c) -> "Player 1's state: " ++ show a ++ " antidotes, " ++ show c ++ " coins."] ++
  [case p2State of
    Nothing          -> "Player 2 dies."
    Just (PS_ac a c) -> "Player 2's state: " ++ show a ++ " antidotes, " ++ show c ++ " coins."] ++
  (if isGameEnd gs 
   then [] 
   else ["Maximum number of cells can be chomped in one move is " ++ show maxChomp ++ "."])

-- Task 3
-- Determine whether a move is legal.
isLegalMove :: GameState -> Int -> Int -> Bool
isLegalMove (numInitRows, numInitCols, maxChomp, gameBoard, _, _, _) r c =
  r >= 0 && r < numInitRows &&
  c >= 0 && c < numInitCols &&
  r < length gameBoard &&
  c < length (gameBoard !! r) && 
  sum [length (drop c row) | row <- take (r+1) gameBoard] <= maxChomp
  
-- Task 4
-- Update game state after a legal move.
makeMove :: GameState -> Int -> Int -> GameState
makeMove (numInitRows, numInitCols, maxChomp, gameBoard, player, p1State, p2State) r c =
  (numInitRows, numInitCols, maxChomp, newBoard, nextPlayer, newP1State, newP2State)
  where
    dropRight i = drop c (gameBoard !! i)
    chompedCells = concat (map dropRight [0..r])
    updateRow i = 
      if i <= r then take c (gameBoard !! i) else gameBoard !! i
    newBoard = map updateRow [0..numInitRows-1]
    nextPlayer = if player == P1 then P2 else P1 
    newP1State = if player == P1 
                 then case p1State of
                   Nothing -> Nothing
                   Just (PS_ac a c) -> adjustPlayerState chompedCells (PS_ac a c)
                 else p1State
    newP2State = if player == P2 
                 then case p2State of
                   Nothing -> Nothing
                   Just (PS_ac a c) -> adjustPlayerState chompedCells (PS_ac a c)
                 else p2State
                
-- Task 5
-- | Print out a message to indicate which player wins, or that it is a draw. 
-- >>> endGameMsg (3, 3, 5, [], P2, Nothing, Just (PS_ac 2 10))
-- "Player 2 wins because Player 1 dies."
-- >>> endGameMsg (3, 3, 5, [], P1, Just (PS_ac 2 20), Just (PS_ac 1 10))
-- "Player 1 wins because Player 1 has 20 coins, which are more than 10 coins of Player 2."
-- >>> endGameMsg (3, 3, 5, [], P1, Just (PS_ac 2 10), Just (PS_ac 3 10))
-- "The game ends up with a draw because both players have 10 coins."
endGameMsg :: GameState -> String
endGameMsg (_, _, _, _, _, Nothing, Just _) = "Player 2 wins because Player 1 dies."
endGameMsg (_, _, _, _, _, Just _, Nothing) = "Player 1 wins because Player 2 dies."
endGameMsg (_, _, _, _, _, Just (PS_ac _ c1), Just (PS_ac _ c2)) 
  | c1 > c2   = "Player 1 wins because Player 1 has " ++ show c1 ++ " coins, which are more than " ++ show c2 ++ " coins of Player 2."
  | c2 > c1   = "Player 2 wins because Player 2 has " ++ show c2 ++ " coins, which are more than " ++ show c1 ++ " coins of Player 1."
  | otherwise = "The game ends up with a draw because both players have " ++ show c1 ++ " coins."
