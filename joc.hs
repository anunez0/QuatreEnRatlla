import System.Random
import Rainbow

{-# LANGUAGE OverloadedStrings #-}

randInt :: Int -> Int -> IO Int
-- randInt low high is an IO action that returns a
-- pseudo-random integer between low and high (both included).

randInt low high = do
    random <- randomIO :: IO Int
    let result = low + random `mod` (high - low + 1)
    return result

-- Slot representa una ranura de tauler.
data Slot = Empty | Red | Yellow
    -- La instanciació automàtica val en aquest cas.
    deriving (Eq)

-- S'utilitza per escriure el tauler.
instance Show Slot where
    show Empty = "O  "
    show Red = "R  "
    show Yellow = "Y  "

-- Board representa un tauler com una matriu de Slot.
data Board = Board [[Slot]]

-- showIndexCol n afegeix el nombre d'espais necessaris a n perquè el tauler s'escrigui correctament.
showIndexCol :: Int -> String
showIndexCol n
    | n < 10    = (show n) ++ "  "
    | otherwise = (show n) ++ " "

-- Intenta ressemblar un tauler físic. S'afegeixen els índexs per facilitar la jugabilitat. Com es pot observar, el tauler està guardat a memòria al revés per facilitar algunes operacions posteriors.
instance Show Board where
    show (Board ([]:cols)) = concat (map showIndexCol [0 .. (length cols)])
    show (Board board) = (concat (map show (map last board))) ++ "\n" ++ (show (Board (map init board)))

-- assignColor s transforma un String en un Slot.
assignColor :: String -> Slot
assignColor "R" = Red
assignColor "Y" = Yellow

-- assignStrategy s transforma un String en una estratègia.
assignStrategy :: String -> (Board -> Slot -> IO Int)
assignStrategy "random" = randomStrategy
assignStrategy "greedy" = greedyStrategy
assignStrategy "smart" = smartStrategy

-- initBoard n m inicialitza un tauler n*m.
initBoard :: Int -> Int -> Board
initBoard n m = Board (take m (repeat (take n (repeat Empty))))

-- printBoard s escriu un tauler amb colors.
printBoard :: String -> IO ()
printBoard [] = do
    putChar '\n'
printBoard (c:cs) = do
    if c == 'R' then do
        putChunk (chunk "R" & fore red)
    else if c == 'Y' then do
        putChunk (chunk "Y" & fore yellow)
    else
        putChar c
    printBoard cs

-- unfilledCols board j retorna els índexs de les columnes no plenes de board. j és un paràmetre recursiu i perquè funcioni correctament s'ha de cridar amb valor 0.
unfilledCols :: Board -> Int -> [Int]
unfilledCols (Board []) j = []
unfilledCols (Board (col:cols)) j
    | last col == Empty = (j:(unfilledCols (Board cols) (j + 1)))
    | otherwise         = unfilledCols (Board cols) (j + 1)

-- getIndexCol board demana un índex de columna a l'usuari i el retorna si és vàlid, és a dir, que la columna no està plena.
getIndexCol :: Board -> IO Int
getIndexCol (Board board) = do
    putStrLn "Columna:"
    indexColStr <- getLine
    let indexCol = (read indexColStr) :: Int
    if not (elem indexCol (unfilledCols (Board board) 0)) then do
        putStrLn "Columna plena"
        getIndexCol (Board board)
    else
        return indexCol

-- insertDiscCol col color introdueix una fitxa de color color a la columna col. Retorna la columna actualitzada.
insertDiscCol :: [Slot] -> Slot -> [Slot]
insertDiscCol [] _ = []
insertDiscCol (Empty:sls) color = (color:sls)
insertDiscCol (sl:sls) color = (sl:(insertDiscCol sls color))

-- insertDisc board color indexCol introdueix una fitxa de color color a la columna indexCol de board. Retorna el tauler actualitzat.
insertDisc :: Board -> Slot -> Int -> Board
insertDisc (Board board) color indexCol = Board ((take indexCol board) ++ [insertDiscCol (board !! indexCol) color] ++ (drop (indexCol + 1) board))

-- lineSize board color i j iDir jDir retorna la mida de la ratlla de color color des de board(i, j) en direcció (iDir, jDir) a board.
lineSize :: Board -> Slot -> Int -> Int -> Int -> Int -> Int
lineSize (Board board) color i j iDir jDir
    | inside && sameColor = 1 + (lineSize (Board board) color (i + iDir) (j + jDir) iDir jDir)
    | otherwise           = 0
    where
        inside = ((i + iDir) >= 0) && ((i + iDir) < n) && ((j + jDir) >= 0) && ((j + jDir) < m)
        n = length (board !! 0)
        m = length board
        sameColor = ((board !! (j + jDir)) !! (i + iDir)) == color

-- topCol col i retorna la fila on cauria una fitxa a la columna col. i és un paràmetre recursiu i perquè funcioni correctament s'ha de cridar amb valor 0.
topCol :: [Slot] -> Int -> Int
topCol [] i = i
topCol (Empty:sls) i = i
topCol (sl:sls) i = topCol sls (i + 1)

-- maximumLineSize board color indexCol retorna la mida de la ratlla més llarga de color color que inclogui l'última fitxa de la columna indexCol a board.
maximumLineSize :: Board -> Slot -> Int -> Int
maximumLineSize (Board board) color indexCol = maximum [horizontal, vertical, diagonal1, diagonal2]
    where
        horizontal = 1 + (lineSize (Board board) color i j 0 (-1)) + (lineSize (Board board) color i j 0 1)
        -- A dalt no hi ha res!
        vertical = 1 + (lineSize (Board board) color i j (-1) 0)
        diagonal1 = 1 + (lineSize (Board board) color i j (-1) (-1)) + (lineSize (Board board) color i j 1 1)
        diagonal2 = 1 + (lineSize (Board board) color i j (-1) 1) + (lineSize (Board board) color i j 1 (-1))
        i = (topCol (board !! indexCol) 0) - 1
        j = indexCol

-- maximumLineSizeLocal board color (i, j) retorna la mida de la ratlla més llarga de color color des de board(i, j) en una de les 4 direccions principals a board. No cal avaluar les altres 4 direccions per l'ús que es fa a maximumLineSizeGlobal.
maximumLineSizeLocal :: Board -> Slot -> (Int, Int) -> Int
maximumLineSizeLocal (Board board) color (i, j) = maximum [horizontal, vertical, diagonal1, diagonal2]
    where
        horizontal = 1 + (lineSize (Board board) color i j 0 (-1))
        vertical = 1 + (lineSize (Board board) color i j (-1) 0)
        diagonal1 = 1 + (lineSize (Board board) color i j (-1) (-1))
        diagonal2 = 1 + (lineSize (Board board) color i j (-1) 1)


-- maximumLineSizeGlobal board color retorna la mida de la ratlla més llarga de color color a board.
maximumLineSizeGlobal :: Board -> Slot -> Int
maximumLineSizeGlobal (Board board) color = maximum (map (maximumLineSizeLocal (Board board) color) tuples)
    where
        tuples = [(i, j) | i <- [0 .. (n - 1)], j <- [0 .. (m - 1)], ((board !! j) !! i) == color]
        n = length (board !! 0)
        m = length board

-- fullBoard board retorna si el board està ple.
fullBoard :: Board -> Bool
fullBoard (Board board) = not (elem Empty (map last board))

-- notColor color retorna el color que no és color.
notColor :: Slot -> Slot
notColor Red = Yellow
notColor Yellow = Red

-- mapMaximumLineSize board boards color j fa un map de maximumLineSize a boards i si la columna associada a un tauler de boards està plena a board (el tauler original), posa el màxim a 0.
mapMaximumLineSize :: Board -> [Board] -> Slot -> Int -> [Int]
mapMaximumLineSize (Board board) [] color j = []
mapMaximumLineSize (Board board) (newBoard:boards) color j
    | elem j (unfilledCols (Board board) 0) = (max:(mapMaximumLineSize (Board board) boards color (j + 1)))
    | otherwise                             = (0:(mapMaximumLineSize (Board board) boards color (j + 1)))
    where
        max = maximumLineSize newBoard color j

-- heuristic board color min retorna el valor heurístic de board respecte a color i min (si es vol minimitzar o maximitzar).
heuristic :: Board -> Slot -> Bool -> Int
heuristic (Board board) color min
    | (not min) && win = 1000
    | min && lose      = -1000
    | otherwise        = 0
    where
        win = (maximumLineSizeGlobal (Board board) color) >= 4
        lose = (maximumLineSizeGlobal (Board board) (notColor color)) >= 4

-- minimax color min depth board implementa l'algoritme de minimax per una profunditat depth.
minimax :: Slot -> Bool -> Int -> Board -> Int
minimax color min depth board
    | leafMin || leafMax = heuristic board color min
    | min                = minimum (map (minimax color (not min) (depth - 1)) boardsMin)
    | otherwise          = maximum (map (minimax color (not min) (depth - 1)) boardsMax)
    where
        boardsMin = map (insertDisc board (notColor color)) (unfilledCols board 0)
        boardsMax = map (insertDisc board color) (unfilledCols board 0)
        leafMin = min && ((depth == 0) || (null boardsMin))
        leafMax = (not min) && ((depth == 0) || (null boardsMax))

-- randomStrategy board color implementa l'estratègia random.
randomStrategy :: Board -> Slot -> IO Int
randomStrategy (Board board) color = do
    let cols = unfilledCols (Board board) 0
    r <- randInt 0 ((length cols) - 1)
    return (cols !! r)

-- greedyStrategy board color implementa l'estratègia greedy.
greedyStrategy :: Board -> Slot -> IO Int
greedyStrategy (Board board) color = do
    let m = length board
    let boardsCPU = map (insertDisc (Board board) color) [0 .. (m - 1)]
    let maximumsCPU = mapMaximumLineSize (Board board) boardsCPU color 0
    let maxCPU = maximum maximumsCPU
    let boardsPlayer = map (insertDisc (Board board) (notColor color)) [0 .. (m - 1)]
    let maximumsPlayer = mapMaximumLineSize (Board board) boardsPlayer (notColor color) 0
    let maxPlayer = maximum maximumsPlayer
    if maxCPU >= 4 then do
        let colsMaxCPU = filter ((== maxCPU) . (maximumsCPU !!)) [0 .. (m - 1)]
        rCPU <- randInt 0 ((length colsMaxCPU) - 1)
        return (colsMaxCPU !! rCPU)
    else if maxPlayer >= 4 then do
        let colsMaxPlayer = filter ((== maxPlayer) . (maximumsPlayer !!)) [0 .. (m - 1)]
        rPlayer <- randInt 0 ((length colsMaxPlayer) - 1)
        return (colsMaxPlayer !! rPlayer)
    else do
        let colsMaxCPU = filter ((== maxCPU) . (maximumsCPU !!)) [0 .. (m - 1)]
        rCPU <- randInt 0 ((length colsMaxCPU) - 1)
        return (colsMaxCPU !! rCPU)

-- smartStrategy board color implementa l'estratègia smart.
smartStrategy :: Board -> Slot -> IO Int
smartStrategy board color = do
    let boardsMax = map (insertDisc board color) (unfilledCols board 0)
    let values = map (minimax color True 4) boardsMax
    let maxValue = maximum values
    let colsMaxValue = filter ((== maxValue) . (values !!)) [0 .. ((length values) - 1)]
    let cols = map ((unfilledCols board 0) !!) colsMaxValue
    r <- randInt 0 ((length cols) - 1)
    return (cols !! r)

-- takeTurnPlayer board color strategy implementa el torn del jugador.
takeTurnPlayer :: Board -> Slot -> (Board -> Slot -> IO Int) -> IO ()
takeTurnPlayer (Board board) color strategy = do
    indexCol <- getIndexCol (Board board)
    let (Board newBoard) = insertDisc (Board board) color indexCol
    printBoard (show (Board newBoard))
    if (maximumLineSize (Board newBoard) color indexCol) >= 4 then do
        putStrLn "Has guanyat :)"
    else if fullBoard (Board newBoard) then do
        putStrLn "Empat :|"
    else
        takeTurnCPU (Board newBoard) (notColor color) strategy

-- takeTurnCPU board color strategy implementa el torn de l'ordinador.
takeTurnCPU :: Board -> Slot -> (Board -> Slot -> IO Int) -> IO ()
takeTurnCPU (Board board) color strategy = do
    putStrLn "Columna:"
    indexCol <- strategy (Board board) color
    putStrLn (show indexCol)
    let (Board newBoard) = insertDisc (Board board) color indexCol
    printBoard (show (Board newBoard))
    if (maximumLineSize (Board newBoard) color indexCol) >= 4 then do
        putStrLn "Has perdut :("
    else if fullBoard (Board newBoard) then do
        putStrLn "Empat :|"
    else
        takeTurnPlayer (Board newBoard) (notColor color) strategy

main :: IO ()
-- main program that runs "Quatre en ratlla".

main = do
    putStrLn "   ___              _                                     _   _ _       "
    putStrLn "  / _ \\ _   _  __ _| |_ _ __ ___    ___ _ __    _ __ __ _| |_| | | __ _ "
    putStrLn " | | | | | | |/ _` | __| '__/ _ \\  / _ \\ '_ \\  | '__/ _` | __| | |/ _` |"
    putStrLn " | |_| | |_| | (_| | |_| | |  __/ |  __/ | | | | | | (_| | |_| | | (_| |"
    putStrLn "  \\__\\_\\\\__,_|\\__,_|\\__|_|  \\___|  \\___|_| |_| |_|  \\__,_|\\__|_|_|\\__,_|"
    putStrLn "                                                                        "
    putStrLn "Defineix n: (1 <= n <= 100)"
    nStr <- getLine
    let n = (read nStr) :: Int
    putStrLn "Defineix m: (1 <= m <= 100)"
    mStr <- getLine
    let m = (read mStr) :: Int
    putStrLn "Tria un color: (R/Y)"
    colorStr <- getLine
    let color = assignColor colorStr
    putStrLn "Defineix l'estrategia: (random/greedy/smart)"
    strategyStr <- getLine
    let strategy = assignStrategy strategyStr
    let (Board board) = initBoard n m
    printBoard (show (Board board))
    r <- randInt 0 1
    if r == 0 then do
        putStrLn "Comences"
        takeTurnPlayer (Board board) color strategy
    else do
        putStrLn "Comenca l'ordinador"
        takeTurnCPU (Board board) (notColor color) strategy