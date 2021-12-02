import Control.Monad
import System.IO

main = do
	input <- readInput
	let (x,y) = (move (0,0,0) input)
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

move :: (Int,Int,Int) -> [(String, Int)] -> (Int,Int)

move (x,y,_) [] = (x,y)
move (x,y,a) ((direction, n):rest) = move 
	(case direction of
		"forward" -> (x+n, y+(n*a), a)
		"up" -> (x, y, a-n)
		"down" -> (x, y, a+n)
		_-> error "invalid direction") rest

