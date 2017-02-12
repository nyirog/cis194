import Data.Char (isControl, isDigit)
import Data.List (groupBy)

halveEvens :: [Integer] -> [Integer]
halveEvens [] = []
halveEvens xs = [x `div` 2| x <- xs, (x `mod` 2) == 0]

safeString :: String -> String
safeString [] = []
safeString cs = map check cs
  where
    check :: Char -> Char
    check c = if isControl c then '_' else c


holes :: [a] -> [[a]]
holes [] = []
holes xs = [select i | i <- [1 .. length xs]]
  where
    select i = [x | (i', x) <- enumerate, i /= i'] 
    enumerate = zip [1..] xs

longestText :: Show a => [a] -> a
longestText = foldl1 go
  where
    go acc x = if length (show x) > length (show acc) then x else acc

adjacents :: [a] -> [(a,a)]
adjacents xs = zip xs (drop 1 xs)

commas :: [String] -> String
commas [] = []
commas xs = foldl1 (\acc x -> acc ++ ", " ++ x) xs

addPolynomials :: [[Integer]] -> [Integer]
addPolynomials  = foldl1 (\acc x -> zipWith (+) acc x)

sumNumbers :: String -> Integer
sumNumbers xs = sum [read x | x <- groupBy (\a b -> (isDigit a) == (isDigit b)) xs, all isDigit x]

