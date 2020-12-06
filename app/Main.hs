module Main where

import Day06 (firstHalf, secondHalf)
import Lib (dispDay, dispHeader)

main :: IO ()
main = do
  dispHeader
  dispDay 6 "Custom Customs" Day06.firstHalf Day06.secondHalf
