import Data.List (foldl')

main :: IO ()
main = do
    contents <- readFile "data.txt"
    let eqs = map (break (== ':')) $ lines contents
        part1 = testOps [(+), (*)] eqs
        part2 = testOps [(+), (*), (+++)] eqs

    mapM_ (print . sum . map (\(l, _:r) -> read l :: Int)) [part1, part2]

testOps :: [Int -> Int -> Int] -> [(String, String)] -> [(String, String)]
testOps ops =
    filter (\(l, _:r) ->
        let res = read l :: Int
            (arg1:args) = map read $ words r :: [Int]
        in test res arg1 args)
    where
        test :: Int -> Int -> [Int] -> Bool
        test res acc [] = res == acc
        test res acc (x:xs) =
            any (\op -> let acc' = op acc x
                        in acc' <= res && test res acc' xs) ops

(+++) :: Int -> Int -> Int
x +++ y = read $ show x ++ show y
