module Day06 (firstHalf, secondHalf) where

import Data.List.Split (splitOn)
import Data.List.Unique

firstHalf :: IO Int
firstHalf = do
  s <- readFile "inputs/day06.txt"
  return $ sum $ map (length . sortUniq . filter (/= '\n')) (splitOn "\n\n" s)

secondHalf :: Integer
secondHalf = 0