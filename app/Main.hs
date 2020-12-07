module Main where

import Day06 (firstHalf, secondHalf)
import Day07 (firstHalf, secondHalf)
import Lib (dispDay, dispHeader)

main :: IO ()
main = do
  dispHeader
  dispDay 6 "Custom Customs" Day06.firstHalf Day06.secondHalf
  dispDay 7 "Handy Haversacks" Day07.firstHalf Day07.secondHalf
