module Main where

import Data.List;
import Data.Char (isDigit);
import Data.List.Split;

countColour :: String -> [String] -> Int
countColour colour draws = sum $ map read $ map (filter (isDigit)) $ filter (\x -> colour `isInfixOf` x) draws

checkDraw :: String -> Bool
checkDraw s = countRed <= 12 && countGreen <= 13 && countBlue <= 14
    where
        counts = splitOn "," s 
        countRed = countColour "red" counts
        countGreen = countColour "green" counts
        countBlue = countColour "blue" counts


validGame :: String -> Bool
validGame s = foldr (&&) True $ map checkDraw $ splitOn ";" (drop 1 $ dropWhile (/= ':') s)    

checkLines :: [String] -> Int
checkLines xs = sum $ map read $ map (drop 1) $ map (dropWhile (/= ' ')) $ map (takeWhile (/= ':')) $ filter validGame xs

main :: IO ()
main = do
    content <- readFile("input")
    let linesOfFile = lines content
    let res = checkLines linesOfFile
    print (res)
