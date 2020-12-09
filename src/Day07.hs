{-# LANGUAGE TupleSections #-}

module Day07 (firstHalf, secondHalf) where

import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.ParserCombinators.Parsec
  ( GenParser,
    many,
    noneOf,
    parse,
    sepBy,
    skipMany,
    space,
    string,
    (<|>),
  )
import Text.ParserCombinators.Parsec.Number (decimal)

data Content
  = ContainNoOtherBags String
  | Contain String [(Int, String)]
  deriving (Show, Read, Eq, Ord)

getColor :: Content -> String
getColor (ContainNoOtherBags color) = color
getColor (Contain color _) = color

toKeyValue :: Content -> (String, [(Int, String)])
toKeyValue (ContainNoOtherBags color) = (color, [])
toKeyValue (Contain color contents) = (color, contents)

noOtherBags :: String -> GenParser Char st Content
noOtherBags color = do
  string "no other bags"
  return $ ContainNoOtherBags color

quantity :: GenParser Char st (Int, String)
quantity = do
  n <- decimal
  skipMany space
  color <- many (noneOf ",.")
  let bagOrBags = if n == 1 then 4 else 5
  return (n, reverse $ drop bagOrBags $ reverse color) -- removes " bags" or " bag"

contain :: String -> GenParser Char st Content
contain color = do
  quantities <- quantity `sepBy` string ", "
  return $ Contain color quantities

content :: String -> GenParser Char st Content
content color = do
  noOtherBags color <|> contain color

parseLine :: [[Char]] -> Content
parseLine [color, contents] =
  case parse (content color) "" contents of
    Right v -> v
    Left _ -> ContainNoOtherBags ""
parseLine _ = ContainNoOtherBags ""

getInput :: IO [Content]
getInput = do
  s <- readFile "inputs/day07.txt"
  let lines = map (splitOn " bags contain ") $ splitOn "\n" s
  return $ map parseLine lines

contains :: String -> Content -> Bool
contains _ (ContainNoOtherBags _) = False
contains _ (Contain _ []) = False
contains color (Contain c ((_, c') : cs)) = (c' == color) || contains color (Contain c cs)

howManyColorsContains :: [Content] -> String -> Int
howManyColorsContains contents color =
  f Set.empty [color] (Set.fromList contents)
  where
    f :: Set Content -> [String] -> Set Content -> Int
    f canContain [] _ = Set.size canContain
    f canContain (color : colors) contentsToCheck
      | null contentsToCheck = Set.size canContain
      | otherwise =
        let canContainColor = Set.filter (contains color) contentsToCheck
         in let newColorsToCheck = map getColor $ Set.toList canContainColor
             in f
                  (canContain `Set.union` canContainColor)
                  (colors ++ newColorsToCheck)
                  (contentsToCheck `Set.difference` canContainColor)

-- howManyBagsInside :: [Content] -> String -> Integer
-- howManyBagsInside contents color =
--   let bag = lookupInContentsAL color
--        in f 0 bag
--   where
--     contentsAL = map toKeyValue contents
--     lookupInContentsAL :: String -> (String, [(String,Int)])
--     lookupInContentsAL color = (color, fromMaybe [] $ lookup color contentsAL)
--     f :: Integer -> (String, [(String, Int)]) -> Integer
--     f acc (_, []) = acc + 1
--     f acc (color, qs) =
--       let f' (c, n) = n * f 0 (lookupInContentsAL c) in
--         sum $ map f' qs

firstHalf :: IO Integer
firstHalf = do
  contents <- getInput
  -- print contents
  return $ toInteger $ howManyColorsContains contents "shiny gold"

secondHalf :: IO Integer
secondHalf = do
  contents <- getInput
  return $ toInteger 0 --howManyBagsInside contents "shiny gold"