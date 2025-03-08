import Data.Map (Map)
import Data.Map qualified as M

main :: IO ()
main = do
  contents <- readFile "data.txt"

  let stones = words contents
      [part1, part2] =
        map
          ( foldr
              ( \_ acc ->
                  foldr
                    ( \(k, v) acc2 ->
                        if null k || k == "0"
                          then M.insertWith (+) "1" v $ M.adjust (\a -> a - v) k acc2
                          else
                            if even $ length k
                              then
                                let (a, b) = splitAt (length k `div` 2) k
                                 in M.insertWith (+) (dropWhile (== '0') b) v $
                                      M.insertWith (+) a v $
                                        M.adjust (\a -> a - v) k acc2
                              else
                                M.insertWith (+) (show (2024 * read k)) v $
                                  M.adjust (\a -> a - v) k acc2
                    )
                    acc
                    (M.toList acc)
              )
              (foldr (\x acc -> M.insertWith (+) x 1 acc) M.empty stones)
          )
          [[1 .. 25], [1 .. 75]]

  print $ M.foldr (+) 0 part1
  print $ M.foldr (+) 0 part2
