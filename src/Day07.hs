module Day07 where

import Data.List.Split (splitOn)
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
  deriving (Show, Read, Eq)

noOtherBags :: String -> GenParser Char st Content
noOtherBags color = do
  string "no other bags"
  return $ ContainNoOtherBags color

quantity :: GenParser Char st (Int, String)
quantity = do
  n <- decimal
  skipMany space
  color <- many (noneOf ",.")
  return (n, reverse $ drop 5 $ reverse color) -- removes " bags"

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

firstHalf :: IO Int
firstHalf = do
  i <- getInput
  print i
  return 0

secondHalf :: IO Int
secondHalf = do
  return 0