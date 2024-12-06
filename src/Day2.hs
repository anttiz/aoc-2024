module Day2 where

-- Function to check if a list of levels is safe
isSafe :: [Int] -> Bool
isSafe levels = (isIncreasing levels || isDecreasing levels) && allPairsValid pairs
  where
    pairs = zip levels (tail levels)

-- Function to check if the list is increasing
isIncreasing :: [Int] -> Bool
isIncreasing levels = all (uncurry (<)) pairs
  where
    pairs = zip levels (tail levels)

-- Function to check if the list is decreasing
isDecreasing :: [Int] -> Bool
isDecreasing levels = all (uncurry (>)) pairs
  where
    pairs = zip levels (tail levels)

-- Function to validate pairs
allPairsValid :: [(Int, Int)] -> Bool
allPairsValid pairs = all (\(x, y) -> abs (x - y) >= 1 && abs (x - y) <= 3) pairs

-- Function to count safe reports
countSafeReports :: [[Int]] -> Int
countSafeReports reports = length $ filter isSafe reports

-- Function to check if a report is safe with one number removed
isSafeWithOneRemoved :: [Int] -> Bool
isSafeWithOneRemoved report = any isSafe (removeElements report)

-- Function to count safe reports with one number removed
countSafeReportsPart2 :: [[Int]] -> Int
countSafeReportsPart2 reports = length $ filter isSafeWithOneRemoved reports

-- New helper function to generate reports with one element removed
removeElements :: [Int] -> [[Int]]
removeElements report = map removeElement [0 .. length report - 1]
  where
    removeElement index = take index report ++ drop (index + 1) report

-- Function to read reports from a file
readReports :: FilePath -> IO [[Int]]
readReports filePath = do
  contents <- readFile filePath
  let reports = map (map read . words) (lines contents) -- Split lines into words and convert to Int
  return reports

result2 :: IO ()
result2 = do
  reports <- readReports "src/input/2.txt"
  let result = countSafeReports reports
  putStrLn $ "Total safe reports: " ++ show result

result2Part2 :: IO ()
result2Part2 = do
  reports <- readReports "src/input/2.txt"
  let result = countSafeReportsPart2 reports
  putStrLn $ "Total safe reports with one number removed: " ++ show result
