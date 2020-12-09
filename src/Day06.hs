module Day06 (firstHalf, secondHalf) where

import Data.List (intersect)
import Data.List.Split (splitOn)
import Data.List.Unique (sortUniq)

firstHalf :: IO Integer
firstHalf = do
  s <- readFile "inputs/day06.txt"
  return $ toInteger $ sum $ map (length . sortUniq . filter (/= '\n')) $ splitOn "\n\n" s

secondHalf :: IO Integer
secondHalf = do
  s <- readFile "inputs/day06.txt"
  let questions = ['a' .. 'z']
  return $ toInteger $ sum $ map (length . foldl intersect questions . splitOn "\n") $ splitOn "\n\n" s