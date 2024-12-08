module Day4 where

import Data.List (isPrefixOf, transpose)

countWords :: String -> [String] -> Int
countWords target = sum . map (countNonOverlapping target)
  where
    countNonOverlapping target = count 0
      where
        count acc [] = acc
        count acc l@(x:xs)
          | target `isPrefixOf` l = count (acc + 1) (drop (length target) l)
          | otherwise = count acc xs

reverseLines :: [String] -> [String]
reverseLines = map reverse

diagonalsTLBR :: [[a]] -> [[a]]
diagonalsTLBR [] = []
diagonalsTLBR xss = [ [xss !! (i + k) !! k | k <- [0..n-i-1]] | i <- [0..n-1] ]
                  ++ [ [xss !! k !! (i + k) | k <- [0..n-i-1]] | i <- [1..n-1] ]
  where n = length xss

diagonalsTRBL :: [[a]] -> [[a]]
diagonalsTRBL [] = []
diagonalsTRBL xss = [ [xss !! (i + k) !! (n-k-1) | k <- [0..n-i-1]] | i <- [0..n-1] ]
                  ++ [ [xss !! k !! (n-i-k-1) | k <- [0..n-i-1]] | i <- [1..n-1] ]
  where n = length xss

calcResult :: String -> [String] -> Int
calcResult target lines = sum
  [ countWords target lines
  , countWords target (reverseLines lines)
  , countWords target (transpose lines)
  , countWords target (reverseLines (transpose lines))
  , countWords target (diagonalsTLBR lines)
  , countWords target (reverseLines (diagonalsTLBR lines))
  , countWords target (diagonalsTRBL lines)
  , countWords target (reverseLines (diagonalsTRBL lines))
  ]

result4 :: IO ()
result4 = do
  content <- readFile "src/input/4.txt"
  let xmas = "XMAS"
  let linesRead = lines content
  let result = calcResult xmas linesRead
  putStrLn $ "Result: " ++ show result

calcResultXShape :: String -> [String] -> Int
calcResultXShape target lines = countXShapes target lines
  where
    n = length lines
    m = length (head lines)

countXShapes :: String ->[String] -> Int
countXShapes target xss = sum [countInXShape target xss i j | i <- [1..n-2], j <- [1..m-2]]
  where
    n = length xss
    m = length (head xss)

countInXShape :: String -> [String] -> Int -> Int -> Int
countInXShape target xss i j
  | isXShape target xss i j = 1
  | otherwise = 0

isXShape :: String -> [String] -> Int -> Int -> Bool
isXShape target xss i j = all (isValidIndex xss) positions &&
                           length target == 3 &&
                           any ($ (xss, i, j)) shapeChecks
  where
    positions = [(i, j), (i-1, j-1), (i+1, j-1), (i-1, j+1), (i+1, j+1)]
    shapeChecks = [checkShape1, checkShape2, checkShape3, checkShape4]
    checkShape1 (xss, i, j) = (getCharAtPosition xss i j == target !! 1) &&
                               (getCharAtPosition xss (i-1) (j-1) == head target) &&
                               (getCharAtPosition xss (i+1) (j-1) == last target) &&
                               (getCharAtPosition xss (i-1) (j+1) == head target) &&
                               (getCharAtPosition xss (i+1) (j+1) == last target)
    checkShape2 (xss, i, j) = (getCharAtPosition xss i j == target !! 1) &&
                               (getCharAtPosition xss (i-1) (j-1) == head target) &&
                               (getCharAtPosition xss (i+1) (j-1) == head target) &&
                               (getCharAtPosition xss (i-1) (j+1) == last target) &&
                               (getCharAtPosition xss (i+1) (j+1) == last target)
    checkShape3 (xss, i, j) = (getCharAtPosition xss i j == target !! 1) &&
                               (getCharAtPosition xss (i-1) (j-1) == last target) &&
                               (getCharAtPosition xss (i+1) (j-1) == last target) &&
                               (getCharAtPosition xss (i-1) (j+1) == head target) &&
                               (getCharAtPosition xss (i+1) (j+1) == head target)
    checkShape4 (xss, i, j) = (getCharAtPosition xss i j == target !! 1) &&
                               (getCharAtPosition xss (i-1) (j-1) == last target) &&
                               (getCharAtPosition xss (i+1) (j-1) == head target) &&
                               (getCharAtPosition xss (i-1) (j+1) == last target) &&
                               (getCharAtPosition xss (i+1) (j+1) == head target)

getCharAtPosition :: [String] -> Int -> Int -> Char
getCharAtPosition xss i j
  | isValidIndex xss (i, j) = (xss !! i) !! j
  | otherwise = ' '

isValidIndex :: [String] -> (Int, Int) -> Bool
isValidIndex xss (i, j) = i >= 0 && i < length xss && j >= 0 && j < length (head xss)

result4Part2 :: IO ()
result4Part2 = do
  content <- readFile "src/input/4.txt"
  let linesRead = lines content
  let result = calcResultXShape "MAS" linesRead
  putStrLn $ "Result: " ++ show result

  -- let contentTest = ".M.S......\n..A..MSMS.\n.M.S.MAA..\n..A.ASMSM.\n.M.S.M....\n..........\nS.S.S.S.S.\n.A.A.A.A..\nM.M.M.M.M.\n.........."
  -- let linesReadTest = lines contentTest
  -- let resultTest = calcResultXShape "MAS" linesReadTest
  -- putStrLn $ "Result: " ++ show resultTest

  -- let linesReadTest = ["mbs", "bab", "mbs"]
  -- let resultTest = calcResultXShape "mas" linesReadTest
  -- putStrLn $ "Result: " ++ show resultTest
  -- let result = isXShape "mas" linesReadTest 1 1
  -- putStrLn $ "Result x shape: " ++ show result
