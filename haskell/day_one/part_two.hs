import Data.List (group, sort)

parseInput :: String -> ([Int], [Int])
parseInput input = unzip $ map parseLine $ lines input
  where
    parseLine line = case words line of
      [x, y] -> (read x, read y)
      _ -> error "Invalid input format"

countOccurrences :: Int -> [Int] -> Int
countOccurrences x = length . filter (== x)

calculateScore :: Int -> [Int] -> Int
calculateScore x rightList = x * countOccurrences x rightList

calculateTotalScore :: [Int] -> [Int] -> Int
calculateTotalScore leftList rightList = 
    sum [calculateScore x rightList | x <- leftList]

solve :: String -> Int
solve input = 
    let (leftList, rightList) = parseInput input
    in calculateTotalScore leftList rightList

main :: IO ()
main = do
    input <- readFile "day_one/input.txt"
    let result = solve input
    print result