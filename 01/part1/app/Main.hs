module Main where

import Data.List
import Data.Maybe
import Data.Char

calibrateLine :: String -> Maybe Int
calibrateLine s =   find (\x -> isDigit x) s >>= \first_digit ->
                    find (\x -> isDigit x) (reverse s) >>= \second_digit ->
                    return $ (read $ first_digit:[second_digit]) 

calibrate :: [String] -> Int
calibrate xs = sum (map (\x -> fromJust $ calibrateLine x) xs) 

main :: IO ()
main = do
    content <- readFile("input")
    let linesOfFile = lines content
    let res = calibrate linesOfFile
    print (res)
