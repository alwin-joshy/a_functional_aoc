module Main where

import Data.List
import Data.Maybe
import Data.Char

isDigitOrDigitString :: String -> Maybe Char
isDigitOrDigitString [] = Nothing
isDigitOrDigitString s 
    | "zero" `isPrefixOf` s = Just '0'
    | "one" `isPrefixOf` s = Just '1'
    | "two" `isPrefixOf` s = Just '2'
    | "three" `isPrefixOf` s = Just '3'
    | "four" `isPrefixOf` s = Just '4'
    | "five" `isPrefixOf` s = Just '5'
    | "six" `isPrefixOf` s = Just '6'
    | "seven" `isPrefixOf` s = Just '7'
    | "eight" `isPrefixOf` s = Just '8'
    | "nine" `isPrefixOf` s = Just '9'
    | isDigit $ head s  = Just $ head s
    | otherwise = Nothing

convertToDigits :: String -> [Char]
convertToDigits s = mapMaybe isDigitOrDigitString (tails s)

calibrateLine :: String -> Int
calibrateLine line = read $ first_digit:[second_digit]
    where
        digits = convertToDigits line
        first_digit = head digits
        second_digit = last digits

calibrate :: [String] -> Int
calibrate s = sum $ map (\x -> calibrateLine x) s

main :: IO ()
main = do
    content <- readFile("input")
    let linesOfFile = lines content
    print $ calibrate linesOfFile
