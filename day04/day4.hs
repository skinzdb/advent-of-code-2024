elemAt :: Int -> [a] -> Maybe a
elemAt i xs = if i >= 0 && i < length xs then Just (xs !! i) else Nothing

part1 :: IO ()
part1 = do
  contents <- readFile "data.txt"
  let rows = lines contents
      matches =
        concatMap
          ( \y ->
              concatMap
                ( \x ->
                    filter
                      (\dir -> search (x, y) dir "XMAS" rows)
                      [(1, 0), (-1, 0), (0, 1), (0, -1), (1, 1), (-1, 1), (-1, -1), (1, -1)]
                )
                [0 .. length (head rows) - 1]
          )
          [0 .. length rows - 1]

  print $ length matches

search :: (Int, Int) -> (Int, Int) -> String -> [String] -> Bool
search (x, y) (dx, dy) needle hay = srch 0 needle
  where
    srch _ [] = True
    srch n (c : cs) =
      let (nx, ny) = (x + n * dx, y + n * dy)
       in Just c == (elemAt ny hay >>= elemAt nx) && srch (n + 1) cs

part2 :: IO ()
part2 = do
  contents <- readFile "data.txt"
  let rows = lines contents
      matches = concatMap (\y -> filter (\x -> mas (x, y) rows) [0 .. length (head rows)]) [0 .. length rows - 1]
  print $ length matches

mas :: (Int, Int) -> [String] -> Bool
mas (x, y) hay =
  Just 'A' == (elemAt y hay >>= elemAt x)
    && any
      ( \[tl, br, bl, tr] ->
          Just tl == (elemAt (y + 1) hay >>= elemAt (x - 1))
            && Just br == (elemAt (y - 1) hay >>= elemAt (x + 1))
            && Just bl == (elemAt (y - 1) hay >>= elemAt (x - 1))
            && Just tr == (elemAt (y + 1) hay >>= elemAt (x + 1))
      )
      ["MSMS", "MSSM", "SMMS", "SMSM"]
