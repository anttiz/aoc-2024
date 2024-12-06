module Day1Part2 where

import Data.Map (Map, fromListWith, toList)
import Day1 (readInput)

-- similarityScore :: (Ord a, Num a, Integral a) => [a] -> [a] -> a
-- write function signature
similarityScore :: (Ord a, Num a, Integral a) => [a] -> [a] -> a
similarityScore left right = sum [num * count | num <- left, let count = lookupCount num countMap]
  where
    countMap = countOccurrences right

-- Function to count occurrences of each number in the list
countOccurrences :: (Ord a) => [a] -> Map a Int
countOccurrences right = fromListWith (+) [(x, 1) | x <- right]

-- Lookup function to get the count of a number
-- lookupCount :: (Ord a) => a -> Map a Int -> Int
lookupCount :: (Ord a, Num a) => a -> Map a Int -> a
lookupCount num countMap = case lookup num (toList countMap) of
  Just count -> fromIntegral count
  Nothing -> 0

result1Part2 :: IO ()
result1Part2 = do
  (leftList, rightList) <- readInput "src/input/1.txt"
  let result = similarityScore leftList rightList
  putStrLn $ "Total similarity score: " ++ show result
