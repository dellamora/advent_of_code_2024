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

parseControl :: String -> Maybe (Bool, String)
parseControl ('d':'o':'(':')':rest) = Just (True, rest)
parseControl ('d':'o':'n':'\'':'t':'(':')':rest) = Just (False, rest)
parseControl ('u':'n':'d':'o':'(':')':rest) = Just (True, rest) 
parseControl _ = Nothing


processMemory :: Bool -> String -> Int
processMemory _ [] = 0
processMemory enabled str@(x:xs) = 
  case parseControl str of
    Just (newState, remaining) -> 
      processMemory newState remaining
    Nothing -> 
      case parseMul str of
        Just (result, remaining) -> 
          (if enabled then result else 0) + processMemory enabled remaining
        Nothing -> 
          processMemory enabled xs

solve :: String -> Int
solve = processMemory True  

main :: IO ()
main = do
  contents <- readFile "haskell/day_three/input.txt"
  putStrLn $ "Result: " ++ show (solve contents)