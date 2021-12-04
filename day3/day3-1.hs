import Control.Monad
import System.IO

main = do
	input <- readInput
	let n = length input
	let m = length (input!!0)
	let gamma = (foldl (\a b -> (2*a)+b) 0 (map (\ni -> if ni > div n 2 then 1 else 0) (count '1' input (replicate m 0))))
	let epsilon = (2^m) - (gamma + 1)
	print (gamma*epsilon)

readInput :: IO [(String)]
readInput = do
	line <- getLine
	eof <- isEOF
	if eof then
		return [line]
	else do 
		input <- readInput 
		return (line:input)

count :: Char -> [String] -> [Int] -> [Int]
count c lString lCount = case lString of
			[] -> lCount	
			s:rest -> count c rest (zipWith (+) lCount (map (\ci -> if ci == c then 1 else 0) s))

