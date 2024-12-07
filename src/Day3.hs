module Day3 where

import Text.Regex.PCRE

toInt :: [String] -> (Int, Int)
toInt [x, y] = (read x, read y)
toInt _ = (0, 0)

extractAndSumMul :: String -> Int
extractAndSumMul content = sum [xInt * yInt | [_, x, y] <- matches, let (xInt, yInt) = toInt [x, y]]
  where
    matches = content =~ "mul\\((\\d+),(\\d+)\\)" :: [[String]]

result3 :: IO ()
result3 = do
  content <- readFile "src/input/3.txt"
  let results = extractAndSumMul content
  putStrLn $ "Total sum of multiplications: " ++ show results
