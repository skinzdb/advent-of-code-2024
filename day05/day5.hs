import Data.List (partition, nub, find, sort)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
  contents <- readFile "data.txt"
  let (rules, _ : seqs) = break null $ lines contents
      pmap =
        foldr (\rule acc ->
                let (l, _ : r) = break (== '|') rule
                in M.insertWith (++) r [] $ M.insertWith (++) l [r] acc
              ) M.empty rules
      splitSeqs =
        map (reverse
                . foldl (\(x : xs) y ->
                            if y == ',' then [] : (x : xs) else (x ++ [y]) : xs
                        ) [[]]
            ) seqs
      (ok, bad) = partition (isOrdered pmap) splitSeqs

  -- Part 1
  print $ sum $ map middle ok
  -- Part 2
  print $ sum $ map (\ks -> let m' = M.filterWithKey (\k _ -> k `elem` ks) pmap
                            in middle $ topo m' (leaves m')) bad

middle :: [String] -> Int
middle xs = read $ xs !! (length xs `div` 2)

isOrdered :: Map String [String] -> [String] -> Bool
isOrdered pmap [] = True
isOrdered pmap (x : xs) =
  case M.lookup x pmap of
    Nothing -> isOrdered pmap xs
    Just ys ->
      x `notElem` concat (mapMaybe (`M.lookup` pmap) xs)
        && isOrdered pmap xs

topo :: Map String [String] -> [String] -> [String]
topo _ [] = []
topo m (x : xs) =
    let m' = M.delete x m
    in x : topo m' (nub (leaves m ++ xs))

leaves :: Map String [String] -> [String]
leaves m = [x | x <- M.keys m, x `notElem` concat (M.elems m)]
