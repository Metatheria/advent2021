import Control.Monad
import System.IO

main = do
	input <- readInput
	print (increaseCount input)

readInput :: IO [Int]
readInput = do
	line <- getLine
	eof <- isEOF
	if eof then return [read line]
	else do 
		input <- readInput 
		return ((read line):input)

increaseCount :: [Int] -> Int
increaseCount [] = 0
increaseCount [_] = 0
increaseCount l = 
	let rest = tail l in
		(if sum (take 3 l) < sum(take 3 rest) then 1 else 0) + (increaseCount rest)

