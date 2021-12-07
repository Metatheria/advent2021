import Control.Monad
import System.IO
import Data.List
import Text.Regex.Posix
import Data.List.Index
import Data.Ratio

main = do
	lines <- readInput
	let x_max = maximum (map (\(_,_,x2,_) -> x2) lines)
	let y_max = maximum (map (\(_,y1,_,y2) -> max y1 y2) lines)
	let grid = replicate (x_max+1) (replicate (y_max+1) 0)
	let straightLines = filter (\(x1,y1,x2,y2) -> x1 == x2 || y1 == y2) lines
	print straightLines
	print (sum (map (\col -> length (filter (>= 2) col)) (addLines straightLines grid)))

listToQuadruple :: [Int] -> (Int,Int,Int,Int)
listToQuadruple l = case l of
	[a,b,c,d] -> (a,b,c,d)
	_ -> error "List does not have 4 elements"

readInput :: IO [(Int, Int, Int, Int)]
readInput = do
	eof <- isEOF
	if eof then
		return []
	else do	
		line <- getLine
		input <- readInput
		let (x1,y1,x2,y2) = listToQuadruple (map read (getAllTextMatches $ line =~ "[0-9]+" :: [String]))
		if x1 < x2 || (x1 == x2 && y1 <= y2)  then 
			return ((x1,y1,x2,y2) : input)
		else
			return ((x2,y2,x1,y1) : input)

pointInLine :: (Int,Int) -> (Int,Int,Int,Int) -> Bool
pointInLine (x,y) (x1,y1,x2,y2) =
	if x1 == x2 then (x == x1 && y1 <= y && y <= y2) --vertical line
	else if x == x1 then (y == y1) --non-vertical line (first point)
	else (x1 <= x && x <= x2 && (y-y1)%(x - x1) == (y2-y1)%(x2-x1))

addLines :: [(Int, Int, Int, Int)] -> [[Int]] -> [[Int]]
addLines lines grid = case lines of
	[] -> grid
	l : rest -> addLines rest (imap (\x col -> (imap (\y val -> if pointInLine (x,y) l then val+1 else val) col)) grid)
