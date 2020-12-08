{-# LANGUAGE TupleSections #-}

module Day07 where

import Control.Monad.Fix
import Data.List.Split (splitOn)
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

-- howManyContains :: [Content] -> String -> Int
-- howManyContains contents color =
--   fst (hmc 0 [] contents color 0)
--   where
--     hmc _ checked [] _ acc = (acc, checked)
--     hmc depth checked (ContainNoOtherBags _ : toCheck) color acc =
--       hmc depth checked toCheck color acc
--     hmc depth checked (Contain c cs : toCheck) color acc
--       | depth == 0 && contains (Contain c cs) color =
--         let (acc', checked') = hmc (depth + 1) [] checked c 0
--          in let (acc'', toCheck') = hmc (depth + 1) [] toCheck c 0
--              in hmc depth checked' toCheck' color (acc + 1 + acc' + acc'')
--       | contains (Contain c cs) color =
--         hmc depth checked toCheck color (acc + 1)
--       | otherwise =
--         hmc depth (Contain c cs : checked) toCheck color acc

-- howManyContains' :: [Content] -> String -> Int
-- howManyContains' contents color =
--   howManyContains'' marked1StContainers colorsOf1StContainer
--   where
--     countMarked x = length $ filter snd x
--     markContainers markedContents color =
--       map (\(x, mk) -> (x, contains x color || mk)) markedContents
--     howManyContains'' marked colors =
--       countMarked $ foldl markContainers marked colors

--     marked1StContainers = map (\x -> (x, contains x color)) contents
--     colorsOf1StContainer =
--       map (\(Contain color _, _) -> color) $ filter snd marked1StContainers

howManyColorsContains :: [Content] -> String -> Int
howManyColorsContains contents color =
  f Set.empty [color] (Set.fromList contents)
  where
    getColor :: Content -> String
    getColor (ContainNoOtherBags color) = color
    getColor (Contain color _) = color

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

howManyBagsInside :: [Content] -> String -> Integer
howManyBagsInside contents color =
  0

firstHalf :: IO Int
firstHalf = do
  contents <- getInput
  -- print contents
  return $ howManyColorsContains contents "shiny gold"

secondHalf :: IO Integer
secondHalf = do
  contents <- getInput
  return $ howManyBagsInside contents "shiny gold"