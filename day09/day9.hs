import Data.List (foldl', replicate)

part1 :: IO ()
part1 = do
  contents <- readFile "data.txt"
  let expanded =
        concatMap
          ( \(i, c) ->
              replicate (read [c]) (if even i then i `div` 2 else -1)
          )
          (zip [0 ..] (init contents))

      reverseFiles = reverse $ filter (/= -1) expanded

      defrag =
        take (length reverseFiles) $
          reverse $
            fst $
              foldl'
                ( \(acc, fs) x ->
                    if x == -1 && not (null fs)
                      then (head fs : acc, tail fs)
                      else (x : acc, fs)
                )
                ([], reverseFiles)
                expanded
  print $ sum $ zipWith (*) [0 ..] defrag

part2 :: IO ()
part2 = do
  contents <- readFile "data.txt"
  let expanded =
        zipWith
          ( \i c ->
              if even i
                then (i `div` 2, read [c] :: Int)
                else (-1, read [c] :: Int)
          )
          [0 ..]
          (init contents)

      reverseFiles = reverse $ filter ((/= -1) . fst) expanded

      defrag =
        filter ((/= 0) . snd) $
          foldl'
            ( \acc (i, len) ->
                let (as, bs) = break (\(j, l) -> j == -1 && len <= l) acc
                 in if null bs || i `elem` map fst as
                      then acc
                      else
                        as
                          ++ (i, len)
                          : (-1, snd (head bs) - len)
                          : map (\(k, h) -> if k == i then (-1, len) else (k, h)) (tail bs)
            )
            expanded
            reverseFiles
  print $ sum $ zipWith (\i l -> if l > 0 then i * l else 0) [0 ..] (concatMap (\(i, l) -> replicate l i) defrag)
