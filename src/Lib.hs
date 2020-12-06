module Lib
  ( dispHeader,
    dispDay,
  )
where

dispHeader :: IO ()
dispHeader = do
  putStrLn "Advent of Code"
  putStrLn "   sub y{2020}"
  putStrLn ""
  putStrLn ""

dispDay :: Integer -> String -> IO Int -> Integer -> IO ()
dispDay day title sol1 sol2 = do
  putStrLn $ "--- Day " ++ show day ++ ": " ++ title ++ " ---"
  s1 <- sol1
  putStrLn $ "Your puzzle answer was " ++ show s1
  putStrLn "--- Part Two ---"
  putStrLn $ "Your puzzle answer was " ++ show sol2
