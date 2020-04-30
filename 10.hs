import System.Random

-- MARK: 
-- GAME

main = do
    -- Computer goes first
    let b = initialBoard 5
    step1_check b

-- Creates an empty, SxS, all-0s board.
initialBoard :: Int -> [[Int]]
initialBoard s = take s (repeat (take s (repeat 0)))

step1_check :: [[Int]] -> IO ()
step1_check b =
    if wins b then printWinMessage
    else if loses b then printLoseMessage b (-1)
    else step2_computer b

step2_computer :: [[Int]] -> IO ()
step2_computer b = do
    g <- newStdGen
    let (b', h) = cturn b g
    step3_check b' h

step3_check :: [[Int]] -> Int -> IO ()
step3_check b h =
    if wins b then printWinMessage
    else if loses b then printLoseMessage b h
    else step4_player b h

step4_player :: [[Int]] -> Int -> IO ()
step4_player b h = do
    putStrLn ""
    putStrLn (showboard b h)
    putStrLn "Enter wasd: "
    move <- getLine
    let b' = pturn move b
    -- Only start a new round if something has changed.
    if b == b' then do
        putStrLn "Invalid move."
        step4_player b h
    else 
        step1_check b'

printWinMessage :: IO ()
printWinMessage =  do putStrLn "Nice."

printLoseMessage :: [[Int]] -> Int -> IO ()
printLoseMessage b h = do
    putStrLn ""
    putStrLn (showboard b h)
    putStrLn "Oof, you lose."

--- Show the given board. Highlight the given index.
showboard :: [[Int]] -> Int -> String
showboard b h = 
    let 
        s = length b
    in 
        concat [
        if i `mod` s == 0 then
            if c == 0 then ".\n"
            else if i == (h+1) then (show c)++"*\n"
            else (show c)++"\n"
        else if c == 0 then ". "
        else if i == (h+1) then (show c)++"*"
        else (show c)++" "
        | (i, c)<-(enumerateboard b)]

enumerateboard :: [[Int]] -> [(Int, Int)]
enumerateboard b = zip [1..] (concat b)

-- MARK: 
-- COMPUTER'S TURN

-- Computer's turn
-- Determine empty cells, pick one, and place a "1" there.
-- Takes a board and a random generator.
-- Returns the updated board,
-- and the flatspace index of the added character.
-- Though I think maybe the caller could determine that?
cturn :: [[Int]] -> StdGen -> ([[Int]], Int)
cturn b g =
    let 
        r = random1or2 g
        xs = zeroIndices b
        upperbound = length (xs)
        rs = randomRs (0, upperbound-1) g
        i = rs !! 0
        x = xs !! i 
        s = length b
        index = mapback x s
    in
        -- Replace the random 0 with the random 1 or 2.
        -- Return the updated board as well as the updated index.
        (replace2d index r b, x)

-- Returns 1 or 2.
-- There is an 80% chance that you'll get 1.
random1or2 :: StdGen -> Int
random1or2 g = let 
    rs = randomRs (0 :: Int, 9 :: Int) g
    r = rs !! 0
    in if r > 7 then 2 else 1 -- [8,9] is 20% of [0..9], so 20% chance of getting a 2.

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
-- for an SxS board.
mapback :: Int -> Int -> (Int, Int)
mapback i s =
    let row = quot i s -- i.e. floor of i/s
        col = rem i s -- i.e. remainder of i/s
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
pturn :: [Char] -> [[Int]] -> [[Int]]
pturn ('w':_) b = mvup b
pturn ('a':_) b = mvleft b
pturn ('s':_) b = mvdown b
pturn ('d':_) b = mvright b
pturn _ b       = b

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
loses b = (not (hasNoEmptyCells b))
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
