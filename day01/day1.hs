import Data.List (sort, transpose)

part1 :: IO ()
part1 = do
  contents <- readFile "data.txt"
  let as : bs : _ = map (sort . map read) $ transpose $ map words $ lines contents
      dist = sum $ map abs $ zipWith (-) as bs
  print dist

part2 :: IO ()
part2 = do
  contents <- readFile "data.txt"
  let as : bs : _ = map (map read) $ transpose $ map words $ lines contents
      sim = sum $ map (\a -> a * length (filter (== a) bs)) as
  print sim
