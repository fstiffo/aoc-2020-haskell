module Main where

import Day06
import Day07
import Day09
import Lib (dispDay, dispHeader)

main :: IO ()
main = do
  dispHeader
  dispDay 6 "Custom Customs" Day06.firstHalf Day06.secondHalf
  dispDay 7 "Handy Haversacks" Day07.firstHalf Day07.secondHalf
  dispDay 9 "Encoding Error" Day09.firstHalf Day09.secondHalf
