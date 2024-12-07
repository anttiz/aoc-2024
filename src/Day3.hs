module Day3 where

import Data.List (isPrefixOf)
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

removeBetweenDontAndDo :: String -> String
removeBetweenDontAndDo content = go content
  where
    go remaining
      | null remaining = "" -- Base case: if the remaining string is empty, return an empty string
      | "don't()" `isPrefixOf` remaining =
          let afterDont = drop (length "don't()") remaining
           in go (dropWhile (/= 'd') afterDont) -- Skip until the next 'do()'
      | "do()" `isPrefixOf` remaining =
          let afterDo = drop (length "do()") remaining
           in go afterDo -- Continue after 'do()'
      | otherwise =
          let (str, afterAny) = break (== 'd') remaining -- Capture until the next delimiter
           in str ++ go afterAny -- Accumulate the string and continue

result3Part2 :: IO ()
result3Part2 = do
  content <- readFile "src/input/3.txt"
  let contentWhereDontIsRemoved = removeBetweenDontAndDo content
  let results = extractAndSumMul contentWhereDontIsRemoved
  putStrLn $ "Total sum of multiplications part 2: " ++ show results
