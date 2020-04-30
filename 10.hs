import System.Random

-- MARK: 
-- GAME

main = do
    -- Computer goes first
    let b = initialBoard
    step1_check b

-- Creates an empty, 5x5, all-0s board.
initialBoard :: [[Int]]
initialBoard = take 5 (repeat (take 5 (repeat 0)))

step1_check :: [[Int]] -> IO ()
step1_check b =
    if wins b then do putStrLn "Nice."
    else if loses b then do putStrLn "Oof."
    else step2_computer b

step2_computer :: [[Int]] -> IO ()
step2_computer b = do
    g <- newStdGen
    let b' = cturn b g
    step3_check b'

step3_check :: [[Int]] -> IO ()
step3_check b =
    if wins b then do putStrLn "Nice."
    else if loses b then do putStrLn "Oof."
    else step4_player b

step4_player :: [[Int]] -> IO ()
step4_player b = do
    putStrLn "---"
    p b -- Print the board
    putStr "Enter wasd: "
    move <- getLine
    let b' = pturn (move!!0) b
    step1_check b'

p :: [[Int]] -> IO ()
p board = do putStrLn (showboard board)

showboard :: [[Int]] -> String
showboard board = concat [
    if i `mod` 5 == 0 then
        if c == 0 then ".\n"
        else (show c)++"\n"
    else if c == 0 then ". "
    else (show c)++" "
    | (i, c)<-(enumerateboard board)]

enumerateboard :: [[Int]] -> [(Int, Int)]
enumerateboard board = zip [1..] (concat board)

-- MARK: 
-- COMPUTER'S TURN

-- Computer's turn
-- Determine empty cells, pick one, and place a "1" there.
-- Takes a board and a random generator.
cturn :: [[Int]] -> StdGen -> [[Int]]
cturn b g =
    let 
        xs = zeroIndices b
        upperbound = length (xs)
        rs = randomRs (0, upperbound-1) g
        i = rs !! 0
        x = xs !! i 
        index = mapback x
    in
        replace2d index 1 b

-- Replace item at (row, col) with value in 2d list.
replace2d :: (Int, Int) -> a -> [[a]] -> [[a]]
replace2d _ _ []          = []
replace2d (0, c) a (x:xs) = (replace c a x):xs
replace2d (r, c) a (x:xs) = x:(replace2d (r-1, c) a xs)

-- Replace item at index with value in list.
replace :: Int -> a -> [a] -> [a]
replace _ _ []     = []
replace 0 a (_:xs) = a:xs
replace i a (x:xs) = x:(replace (i-1) a xs)

-- Map the given index from flat space to a (row, col) in 2d space, 
-- assuming a 5x5 board.
mapback :: Int -> (Int, Int)
mapback i =
    let row = quot i 5 -- i.e. floor of i/5
        col = rem i 5 -- i.e. remainder of i/5
    in (row, col)

-- Returns a list of all indices of zero items in the given board;
-- indices are in "flat space".
zeroIndices :: [[Int]] -> [Int]
zeroIndices b = indicesOf (select 0 (enumerate (flatten b)))

-- Converts [value] to [(index, value)].
enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

-- Extracts the indices of an enumerated list.
-- I.e. converts [(index, value)] to [index].
indicesOf :: [(Int, a)] -> [Int]
indicesOf = map fst

-- Converts [[a,b],[c,d]] to [a,b,c,d].
flatten :: [[a]] -> [a]
flatten = concat

-- Selects only 0-valued items from an enumerated list.
select :: Int -> [(Int, Int)] -> [(Int, Int)]
select x xs = filter (\e -> x == (snd e)) xs

-- MARK:
-- PLAYER'S TURN

-- Player's turn.
-- Applies a chosen move (represented by a char*) to the board.
-- * Move is expected to be w,a,s,d.
-- TODO we should probably also make this return whether a move was made? or else the caller can determine wheter b != b'
pturn :: Char -> [[Int]] -> [[Int]]
pturn 'w' b = mvup b
pturn 'a' b = mvleft b
pturn 's' b = mvdown b
pturn 'd' b = mvright b
pturn _ b = b

-- Move a board left
mvleft :: [[Int]] -> [[Int]]
mvleft b = map l b

-- Move board up
mvup :: [[Int]] -> [[Int]]
mvup b = t (mvleft (t b))

-- Move board right
mvright :: [[Int]] -> [[Int]]
mvright b = rh (mvleft (rh b))

-- Move board down
mvdown :: [[Int]] -> [[Int]]
mvdown b = rv (mvup (rv b))

l :: [Int] -> [Int]
l xs = mergeleft (sortleft xs)

mergeleft :: [Int] -> [Int]
mergeleft [] = []
mergeleft [x] = [x]
mergeleft (0:xs) = (mergeleft xs) ++ [0]
mergeleft (x:y:xs) 
    | x==y      = [x+1] ++ (mergeleft xs) ++ [0]
    | otherwise = [x] ++ (mergeleft ([y]++xs))

-- Sorts the given list so that
-- all 0s are at the end.
sortleft :: [Int] -> [Int]
sortleft [] = []
sortleft [x] = [x]
sortleft (0:xs) = (sortleft xs) ++ [0]
sortleft (x:xs) = [x] ++ (sortleft xs)

-- Transposes the board,
-- which is like rotating 90s clockwise
t :: [[a]] -> [[a]]
t ([]:_) = []
t x = (map head x) : t (map tail x)

-- Reverse board horizontally
-- (flip along vertical axis)
rh :: [[a]] -> [[a]]
rh ([]:_) = []
rh b = map reverse b

-- Reverse board vertically
-- (flip along horizontal axis)
rv :: [[a]] -> [[a]]
rv b = reverse b

-- MARK:
-- END CONDITIONS

-- The player wins if the board contains a 10.
wins :: [[Int]] -> Bool
wins b = 10 `elem` (flatten b)

-- The player loses IF:
-- - There are no empty cells, AND
-- - There are no available moves
loses :: [[Int]] -> Bool
loses b = (hasNoEmptyCells b) 
    && (not (anyRowNeighboursMatch b)) 
    && (not (anyColNeighboursMatch b))

-- True if the given board has no empty cells.
hasNoEmptyCells :: [[Int]] -> Bool
hasNoEmptyCells b = 0 `elem` (flatten b)

-- True if the 2d list contains any rows
-- where those rows contain neighbours that match.
anyRowNeighboursMatch :: [[Int]] -> Bool
anyRowNeighboursMatch b = foldl (||) False [listNeighboursMatch row | row <- b]

-- True if the 2d list contains any cols
-- where those cols contain neighbours that match.
anyColNeighboursMatch :: [[Int]] -> Bool
anyColNeighboursMatch b = anyRowNeighboursMatch(t b)

-- True if the list contains any neighbours with equal values,
-- E.g. [1,1,2,3] -> True, but [1,2,3,1] -> False.
listNeighboursMatch :: [Int] -> Bool
listNeighboursMatch [] = False
listNeighboursMatch [x] = False
listNeighboursMatch (x:y:xs)
    | x==y      = True
    | otherwise = listNeighboursMatch (y:xs)
