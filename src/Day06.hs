module Day06 (firstHalf, secondHalf) where

import Data.List (intersect)
import Data.List.Split (splitOn)
import Data.List.Unique (sortUniq)

firstHalf :: IO Int
firstHalf = do
  s <- readFile "inputs/day06.txt"
  return $ sum $ map (length . sortUniq . filter (/= '\n')) $ splitOn "\n\n" s

secondHalf :: IO Int
secondHalf = do
  s <- readFile "inputs/day06.txt"
  let questions = ['a' .. 'z']
  return $ sum $ map (length . foldl intersect questions . splitOn "\n") $ splitOn "\n\n" s