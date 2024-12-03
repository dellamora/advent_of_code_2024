import Data.List (all)

parseLine :: String -> [Int]
parseLine = map read . words

makePairs :: [a] -> [(a, a)]
makePairs [] = []
makePairs [_] = []
makePairs (x:y:rest) = (x,y) : makePairs (y:rest)

isMonotonic :: [Int] -> Bool
isMonotonic [] = True
isMonotonic [_] = True
isMonotonic xs = all increasing pairs || all decreasing pairs
  where
    pairs = makePairs xs
    increasing (a, b) = b - a >= 1 && b - a <= 3
    decreasing (a, b) = a - b >= 1 && a - b <= 3

removeAt :: Int -> [a] -> [a]
removeAt i xs = take i xs ++ drop (i + 1) xs

removeOneElement :: [a] -> [[a]]
removeOneElement xs = [removeAt i xs | i <- [0..length xs - 1]]

isSafeWithDampener :: [Int] -> Bool
isSafeWithDampener xs 
    | isMonotonic xs = True  
    | length xs <= 2 = True 
    | otherwise = any isMonotonic (removeOneElement xs)  

main :: IO ()
main = do
    contents <- readFile "day_two/input.txt"
    let reports = map parseLine (lines contents)
    let safeCount = length $ filter isSafeWithDampener reports
    putStrLn $ "Number of safe reports with Problem Dampener: " ++ show safeCount