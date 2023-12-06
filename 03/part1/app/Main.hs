module Main where

import Data.Char (isDigit, isSymbol);

data Number = Number {
    value :: Int,
    start_pos :: Int,
    end_pos :: Int
}

findSymbols :: Int -> String -> [Int]
findSymbols _ [] = []
findSymbols pos (x:xs)
    | x == '.' || isDigit x = findSymbols (pos + 1) xs 
    | otherwise = pos : findSymbols (pos + 1) xs 

parseNumber :: Int -> Int -> String -> String-> (Number, String)
parseNumber start_pos curr_pos [] curr_num = (Number (read curr_num) start_pos curr_pos, []) 
parseNumber start_pos curr_pos (x:xs) curr_num
    | isDigit x = parseNumber start_pos (curr_pos + 1) xs (curr_num ++ [x]) 
    | otherwise = (Number num start_pos curr_pos, x:xs)
    where
        num = read $ curr_num

inRange :: Int -> Int -> Int -> Bool
inRange lower upper val = val >= lower && val <= upper

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

processLine :: String -> String -> String -> Int
processLine before curr after = sum (map value valid_numbers)
    where
        line_numbers = findNumbers 0 curr
        symbols_prev = findSymbols 0 before
        symbols_curr = findSymbols 0 curr
        symbols_after = findSymbols 0 after
        valid_numbers = filter hasSymbol line_numbers
        hasSymbol number = length (concat $ map (filter (inRange (start_pos number - 1) (end_pos number))) [symbols_prev, symbols_curr, symbols_after]) /= 0

processLines2 :: [String] -> Int
processLines2 (x:y:[]) = processLine x y []
processLines2 (x:y:z:xs) = processLine x y z + processLines2 (y:z:xs)

processLines :: [String] -> Int
processLines [] = 0;
processLines [x] = processLine [] x []
processLines (x:y:[]) = processLine [] x y + processLine x y []
processLines (x:y:xs) = processLine [] x y + processLines2 (x:y:xs)

main :: IO ()
main = do
    content <- readFile("input")
    let linesOfFile = lines content
    let res = processLines linesOfFile
    print res
