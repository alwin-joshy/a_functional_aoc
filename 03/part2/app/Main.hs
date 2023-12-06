module Main where

import Data.Char (isDigit, isSymbol);
import Data.List

data Number = Number {
    value :: Int,
    start_pos :: Int,
    end_pos :: Int
} deriving Show

findGears :: Int -> Int -> String -> [(Int, Int)]
findGears _ _ [] = []
findGears row col (x:xs)
    | x == '*' = (row, col) : findGears row (col + 1) xs 
    | otherwise = findGears row (col + 1) xs 

parseNumber :: Int -> Int -> String -> String-> (Number, String)
parseNumber start_pos curr_pos [] curr_num = (Number (read curr_num) start_pos curr_pos, []) 
parseNumber start_pos curr_pos (x:xs) curr_num
    | isDigit x = parseNumber start_pos (curr_pos + 1) xs (curr_num ++ [x]) 
    | otherwise = (Number num start_pos curr_pos, x:xs)
    where
        num = read $ curr_num

inRange :: Int -> Int -> (Int, Int) -> Bool
inRange lower upper val = snd val >= lower && snd val <= upper

findNumbers :: Int -> String -> [Number]
findNumbers _ [] = []
findNumbers curr_pos (x:xs) = 
    if isDigit x then 
        number : findNumbers (end_pos number) next_string
    else
        findNumbers (curr_pos + 1) xs
    where 
        parse_num = parseNumber curr_pos curr_pos (x:xs) ""
        number = fst parse_num
        next_string = snd parse_num

processLine :: Int -> String -> String -> String -> [(Number, (Int, Int))]
processLine row before curr after = valid_numbers
    where
        line_numbers = findNumbers 0 curr
        possible_prev = findGears (row - 1) 0 before
        possible_curr = findGears (row) 0 curr
        possible_after = findGears (row + 1) 0 after
        valid_numbers = concat $ map hasSymbol line_numbers
        hasSymbol number = map (\x -> (number, x)) (concat $ map (filter (inRange (start_pos number - 1) (end_pos number))) [possible_prev, possible_curr, possible_after])

processLines2 :: Int -> [String] -> [(Number, (Int, Int))]
processLines2 n (x:y:[]) = processLine n x y []
processLines2 n (x:y:z:xs) = processLine n x y z  ++ processLines2 (n+1) (y:z:xs)

processLines :: [String] -> [(Number, (Int, Int))]
processLines [] = [];
processLines [x] = processLine 0 [] x []
processLines (x:y:[]) = processLine 0 [] x y ++ processLine 1 x y []
processLines (x:y:xs) = processLine 0 [] x y ++ processLines2 1 (x:y:xs)

filterGears :: [String] -> Int
filterGears lines =   sum $ 
                map (product . (map ( value . fst))) $ 
                filter (\x -> length x == 2) $ 
                groupBy (\x y -> snd x == snd y) $ 
                sortBy (\x y -> compare (snd x) (snd y)) $ 
                processLines lines

main :: IO ()
main = do
    content <- readFile("input")
    let linesOfFile = lines content
    let res = filterGears linesOfFile
    print res
