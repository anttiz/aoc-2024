module Main where

import Day1 (result1)
import Day1Part2 (result1Part2)
import Day2 (result2, result2Part2)
import Day3 (result3, result3Part2)

main :: IO ()
main = do
  putStrLn "Day 1"
  result1
  putStrLn "Day 1 Part 2"
  result1Part2
  putStrLn "Day 2"
  result2
  putStrLn "Day 2 Part 2"
  result2Part2
  putStrLn "Day 3"
  result3
  putStrLn "Day 3 Part 2"
  result3Part2
