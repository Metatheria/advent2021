import Control.Monad
import System.IO
import Data.Char(digitToInt)

main = do
	input <- readInput
	let n = length input
	let m = length (input!!0)
	print (binToInt (filterMostCommon 0 input)*(binToInt (filterLeastCommon 0 input)))
	

readInput :: IO [(String)]
readInput = do
	line <- getLine
	eof <- isEOF
	if eof then
		return [line]
	else do 
		input <- readInput 
		return (line:input)

binToInt :: String -> Int
binToInt = foldl (\a b -> (2*a) + (digitToInt b)) 0

countChar :: Char -> Int -> [String] -> Int -> Int
countChar c i lString curCount = case lString of
				[] -> curCount	
				s:rest -> countChar c i rest (curCount + (if s!!i == c then 1 else 0))

filterMostCommon :: Int -> [String] -> String
filterMostCommon i lString = case lString of
	[] -> error "No value found"
	[s] -> s
	_ -> let (n,nOnes) = (length lString, countChar '1' i lString 0) in
		filterMostCommon (i+1) (filter (\s -> (s!!i == '1' && nOnes >= div (n+1) 2) || (s!!i == '0' && nOnes < div (n+1) 2)) lString)

filterLeastCommon :: Int -> [String] -> String
filterLeastCommon i lString = case lString of
	[] -> error "No value found"
	[s] -> s
	_ -> let (n,nOnes) = (length lString, countChar '1' i lString 0) in
		filterLeastCommon (i+1) (filter (\s -> (s!!i == '1' && nOnes < div (n+1) 2) || (s!!i == '0' && nOnes >= div (n+1) 2)) lString)
