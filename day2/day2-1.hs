import Control.Monad
import System.IO

main = do
	input <- readInput
	let (x,y) = (move (0,0) input)
	print (x*y)

readInput :: IO [(String, Int)]
readInput = do
	line <- getLine
	eof <- isEOF
	let w = words line
	if eof then
		return [(w!!0, read (w!!1))]
	else do 
		input <- readInput 
		return ((w!!0, read (w!!1)):input)

move :: (Int,Int) -> [(String, Int)] -> (Int,Int)

move (x,y) [] = (x,y)
move (x,y) ((direction, n):rest) = move 
	(case direction of
		"forward" -> (x+n,y)
		"up" -> (x,y-n)
		"down" -> (x,y+n)
		_-> error "invalid direction") rest

