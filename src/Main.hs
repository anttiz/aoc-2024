module Main where

import Day1Part2 (result1Part2)
import Day1 (result1)
import Day2 (result2, result2Part2)
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
