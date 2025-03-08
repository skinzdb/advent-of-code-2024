import Debug.Trace (trace)

{- use 1000 as error value as all other values are 2-digit numbers -}
ascPred :: [Int] -> Bool
ascPred = (/= 1000) . foldl1 (\acc x -> if x - acc `elem` [1 .. 3] then x else 1000)

part1 :: IO ()
part1 = do
  contents <- readFile "data.txt"
  let sequences = map (map read . words) $ lines contents
      safe = filter (\xs -> ascPred xs || ascPred (reverse xs)) sequences
  print $ length safe

part2 :: IO ()
part2 = do
  contents <- readFile "data.txt"
  let sequences = map (map read . words) $ lines contents
      safe = filter (any (\ys -> ascPred ys || ascPred (reverse ys)) . rmOne) sequences
  print $ length safe
  where
    rmOne :: [a] -> [[a]]
    rmOne ys = map ((\(as, bs) -> init as ++ bs) . (`splitAt` ys)) [1 .. length ys]
