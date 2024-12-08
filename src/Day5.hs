module Day5 where

import qualified Data.Map as Map
import Data.List.Split (splitOn)
import Data.List (sortBy)
import Data.Function (on)
-- Define a type for the rules
type RulesMap = Map.Map Int [Int]

-- Function to parse rules into a Map
parseRules :: [String] -> RulesMap
parseRules = foldr insertRule Map.empty
  where
    insertRule rule acc =
      let parts = splitOn "|" rule
      in case parts of
          [keyStr, valueStr] ->
              let key = read keyStr :: Int
                  value = read valueStr :: Int
              in Map.insertWith (++) key [value] acc
          _ -> acc

getMiddle :: [Int] -> Int
getMiddle xs = xs !! (length xs `div` 2)

-- Function to check if an update is valid according to the rules
isValidUpdate :: RulesMap -> [Int] -> Bool
isValidUpdate rulesMap update = all isValidForIndex [0..length update - 1]
  where
    isValidForIndex i =
      let current = update !! i
          subsequent = drop (i + 1) update
      in all (`elem` (Map.findWithDefault [] current rulesMap)) subsequent

-- Function to filter valid updates according to the rules
filterValidUpdates :: RulesMap -> [[Int]] -> [[Int]]
filterValidUpdates rulesMap updates = filter (isValidUpdate rulesMap) updates

result5 :: IO ()
result5 = do
  rules <- readFile "src/input/5_rules.txt"
  updates <- readFile "src/input/5_updates.txt"
  -- rules <- readFile "src/input/5_example_rules.txt"
  -- updates <- readFile "src/input/5_example_updates.txt"
  let rulesList = lines rules
  let updatesList = lines updates

  -- Parse rules into a Map
  let rulesMap = parseRules rulesList

  -- Parse updates into a list of lists of integers
  let parsedUpdates = map (map read . splitOn ",") updatesList :: [[Int]]

  -- Filter valid updates
  let validUpdates = filterValidUpdates rulesMap parsedUpdates
  let sumOfMiddles = sum $ map getMiddle validUpdates

  -- putStrLn $ "Rules: " ++ show (Map.size rulesMap)
  -- putStrLn $ "RulesMap: " ++ show rulesMap
  -- putStrLn $ "Valid Updates: " ++ show validUpdates
  putStrLn $ "Sum of Middles: " ++ show sumOfMiddles

  -- let t = [75,47,61,53,29]
  -- putStrLn $ "T: " ++ show t
  -- putStrLn $ "Valid: " ++ show (isValidUpdate rulesMap t)

  -- putStrLn $ "Updates: " ++ show (length updatesList)
  -- putStrLn $ "Parsed Updates: " ++ show parsedUpdates
  -- putStrLn $ "Result: " ++ show result

-- Function to filter invalid updates according to the rules
filterInvalidUpdates :: RulesMap -> [[Int]] -> [[Int]]
filterInvalidUpdates rulesMap updates = filter (not . isValidUpdate rulesMap) updates

-- Function to filter the rules map based on keys present in the update
filterRulesMap :: RulesMap -> [Int] -> RulesMap
filterRulesMap rulesMap update = updatedRulesMap2
  where
    keysInUpdate = filter (`elem` update) (Map.keys rulesMap)
    updatedRulesMap = Map.filterWithKey (\k _ -> k `elem` keysInUpdate) rulesMap
    -- Filter the values in the rules map to only include those present in the update
    updatedRulesMap2 = Map.map (filter (`elem` update)) updatedRulesMap

fixUpdate :: RulesMap -> [Int] -> [Int]
fixUpdate rulesMap update = map fst sortedUpdates
  where
    updatedRulesMap = filterRulesMap rulesMap update
    sortedUpdates = sortBy (flip compare `on` (length . snd)) [(x, Map.findWithDefault [] x updatedRulesMap) | x <- update]

result5Part2 :: IO ()
result5Part2 = do
  rules <- readFile "src/input/5_rules.txt"
  updates <- readFile "src/input/5_updates.txt"
  -- rules <- readFile "src/input/5_example_rules.txt"
  -- updates <- readFile "src/input/5_example_updates.txt"
  let rulesList = lines rules
  let updatesList = lines updates
  let rulesMap = parseRules rulesList
  let parsedUpdates = map (map read . splitOn ",") updatesList :: [[Int]]
  let invalidUpdates = filterInvalidUpdates rulesMap parsedUpdates
  let fixedUpdates = map (fixUpdate rulesMap) invalidUpdates
  let sumOfMiddles = sum $ map getMiddle fixedUpdates

  -- putStrLn $ "Invalid Updates: " ++ show invalidUpdates
  -- putStrLn $ "Fixed Updates: " ++ show fixedUpdates
  putStrLn $ "Sum of Middles: " ++ show sumOfMiddles
  -- let t = [75,97,47,61,53]
  -- putStrLn $ "T: " ++ show t
  -- let fixedRulesMap = filterRulesMap rulesMap t
  -- putStrLn $ "Fixed Rules Map: " ++ show fixedRulesMap
  -- putStrLn $ "Fixed: " ++ show (fixUpdate fixedRulesMap t)

  -- let t = [97,13,75,29,47]
  -- putStrLn $ "T: " ++ show t
  -- let fixedRulesMap = filterRulesMap rulesMap t
  -- putStrLn $ "Fixed Rules Map: " ++ show fixedRulesMap
  -- putStrLn $ "Fixed: " ++ show (fixUpdate fixedRulesMap t)
