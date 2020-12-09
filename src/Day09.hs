module Day09 where

import Data.List.Split (splitOn)

range :: [a] -> (Int, Int) -> [a]
range ls (i1, i2)
  | i1 >= i2 = []
  | i2 >= length ls = []
  | otherwise = drop i1 $ take (i2 + 1) ls

pairThatGivesSum :: [Int] -> Int -> (Int, Int)
pairThatGivesSum (n : ns) sum =
  f n ns -- returns (0,0) if there is no one that satisfy
  where
    g :: Int -> [Int] -> (Int, Int)
    g _ [] = (0, 0)
    g n (n' : ns)
      | n + n' == sum = (n, n')
      | otherwise = g n ns

    f :: Int -> [Int] -> (Int, Int)
    f _ [] = (0, 0)
    f n (n' : ns)
      | g n (n' : ns) /= (0, 0) = g n (n' : ns)
      | otherwise = f n' ns

findFirstInvalid :: [Int] -> Int -> Int
findFirstInvalid ns preambleLen =
  f preambleLen -- returns 0 if there is no one that satisfy
  where
    f :: Int -> Int
    f i
      | i == length ns = 0
      | otherwise =
        let prevs = drop (i - preambleLen) $ take i ns
            pair = pairThatGivesSum prevs (ns !! i)
         in if pair == (0, 0) then ns !! i else f (i + 1)

findWeakness :: [Int] -> Int -> Int
findWeakness ns invalidNum =
  f 0 -- returns 0 if there is no one that satisfy
  where
    maxI = length ns - 2

    findRangeThatGivesInvalidNum :: Int -> Int -> Int
    findRangeThatGivesInvalidNum i1 i2 -- returns 0 if there is no one that satisfy
      | i2 == maxI + 1 = 0
      | otherwise =
        case sum $ range ns (i1, i2) of
          s | s == invalidNum -> i2
          s | s > invalidNum -> 0
          _ -> findRangeThatGivesInvalidNum i1 (i2 + 1)

    encryptionWeakness :: Int -> Int -> Int
    encryptionWeakness i1 i2 =
      let r = range ns (i1, i2) in minimum r + maximum r

    f :: Int -> Int
    f i
      | i == maxI = 0
      | otherwise = case findRangeThatGivesInvalidNum i (i + 1) of
        i' | i' /= 0 -> encryptionWeakness i i'
        _ -> f (i + 1)

getInput :: IO [Int]
getInput = do
  s <- readFile "inputs/day09.txt"
  return $ map read $ splitOn "\n" s

firstHalf :: IO Integer
firstHalf = do
  input <- getInput
  return $ toInteger $ findFirstInvalid input 25

secondHalf :: IO Integer
secondHalf = do
  input <- getInput
  let invalidNum = findFirstInvalid input 25
  return $ toInteger $ findWeakness input invalidNum