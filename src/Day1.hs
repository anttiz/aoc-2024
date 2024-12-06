module Day1 where

import Data.List (sort)

totalDistance :: [Int] -> [Int] -> Int
totalDistance left right = sum distances
  where
    sortedLeft = sort left
    sortedRight = sort right
    distances = zipWith distance sortedLeft sortedRight
    distance a b = abs (a - b)

readInput :: FilePath -> IO ([Int], [Int])
readInput filePath = do
  contents <- readFile filePath
  let linesOfInput = lines contents
      (left, right) = unzip [(read leftNum, read rightNum) | [leftNum, rightNum] <- map words linesOfInput]
  return (left, right)

result1 :: IO ()
result1 = do
  (leftList, rightList) <- readInput "src/input/1.txt"
  let result = totalDistance leftList rightList
  putStrLn $ "Total distance: " ++ show result
