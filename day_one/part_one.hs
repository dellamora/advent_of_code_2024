import Data.List (sort)

parseInput :: String -> ([Int], [Int])
parseInput input = unzip $ map parseLine $ lines input
  where
    parseLine line = case words line of
      [x, y] -> (read x, read y)
      _ -> error "Invalid input format"

difference :: Int -> Int -> Int
difference x y = abs (x - y)

calculateDistance :: [Int] -> [Int] -> Int
calculateDistance xs ys = sum $ zipWith difference (sort xs) (sort ys)

solve :: String -> Int
solve input = 
  let (leftList, rightList) = parseInput input
  in calculateDistance leftList rightList

-- main :: IO ()
-- main = do
--   input <- readFile "day_one/input.txt"  -- Read from input.txt
--   let result = solve input
--   print result