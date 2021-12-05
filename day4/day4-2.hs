import Control.Monad
import System.IO
import Data.List
import Data.Maybe

main = do
	(numbers, inputGrids) <- readInput
	let grids = (map.map.map) (\n -> (n, False)) inputGrids
	let (winningGrid, finalNumber) = drawUntilLastBingo numbers grids
	print winningGrid
	print finalNumber
	print (finalNumber*((sum (map sum ((map.map) (\(n,drawn) -> if drawn then 0 else n) winningGrid)))))
	
readInput :: IO([Int], [[[Int]]])
readInput = do
	firstLine <- getLine
	grids <- readGrids []
	let drawnNumbers = (map read (words (map (\c -> if c == ',' then ' ' else c) firstLine)))
	return (drawnNumbers, grids)


readGrids :: [[Int]] -> IO [[[Int]]]
readGrids curGrid = do
	line <- getLine
	eof <- isEOF
	if eof then
		return [curGrid ++ [map read (words line)]]
	else if line == "" then 
		if curGrid == [] then
			readGrids []
		else do
			nextGrids <- readGrids []
			return (curGrid : nextGrids)
	else do
		(readGrids (curGrid ++ [map read (words line)]))

draw :: Int -> [[(Int, Bool)]] -> [[(Int, Bool)]]
draw n =  (map.map) (\(i, drawn) -> (i, drawn || i == n))

drawUntilLastBingo :: [Int] -> [[[(Int, Bool)]]] -> ([[(Int, Bool)]], Int)
drawUntilLastBingo numbers grids = case numbers of
	[] -> error "No bingo !"
	n : rest -> let newGrids = map (draw n) grids in
		case (find (\grid -> not (bingo ((map.map) (\(_,a) -> a) grid))) newGrids) of
			Just losingGrid -> drawUntilLastBingo rest newGrids
			Nothing -> (draw n (fromJust (find (\grid -> not (bingo ((map.map) (\(_,a) -> a) grid))) grids)), n) 

bingo :: [[Bool]] -> Bool
bingo grid = (foldl (||) False (map (\row -> foldl (&&) True row) grid) -- rows
	|| foldl (||) False (foldl (zipWith (&&)) (replicate (length grid) True) grid)) --columns
