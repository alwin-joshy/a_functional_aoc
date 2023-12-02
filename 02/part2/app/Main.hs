module Main where

import Data.List;
import Data.Char (isDigit);
import Data.List.Split;

countColour :: String -> [String] -> Int
countColour colour draws = sum $ map (read . filter isDigit) $ filter (\x -> colour `isInfixOf` x) draws

checkDraw :: String -> [Int]
checkDraw s = [countRed, countGreen, countBlue]
    where
        counts = splitOn "," s 
        countRed = countColour "red" counts
        countGreen = countColour "green" counts
        countBlue = countColour "blue" counts

minNumbers :: String -> [Int]
minNumbers s = foldr (zipWith max) [0, 0, 0] $ map checkDraw $ splitOn ";" (drop 1 $ dropWhile (/= ':') s)    

checkLines :: [String] -> Int
checkLines xs = sum $ map (product . minNumbers) xs

main :: IO ()
main = do
    content <- readFile("input")
    let linesOfFile = lines content
    let res = checkLines linesOfFile
    print res
