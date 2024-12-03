import Data.Char (isDigit)

parseNumber :: String -> Maybe (Int, String)
parseNumber [] = Nothing
parseNumber (x:xs)
  | not (isDigit x) = Nothing
  | otherwise = Just (read digits, rest)
  where
    (digits, rest) = span isDigit (x:xs)

parseMul :: String -> Maybe (Int, String)
parseMul ('m':'u':'l':'(':rest) = do
  (n1, afterFirst) <- parseNumber rest
  case afterFirst of
    ',':afterComma -> do
      (n2, afterSecond) <- parseNumber afterComma
      case afterSecond of
        ')':remaining -> Just (n1 * n2, remaining)
        _ -> Nothing
    _ -> Nothing
parseMul _ = Nothing

processMemory :: String -> Int
processMemory [] = 0
processMemory str@(x:xs) = case parseMul str of
  Just (result, remaining) -> result + processMemory remaining
  Nothing -> processMemory xs

solve :: String -> Int
solve = processMemory

main :: IO ()
main = do
  contents <- readFile "haskell/day_three/input.txt"
  putStrLn $ "Result: " ++ show (solve contents)