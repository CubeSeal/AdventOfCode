module Main where

main :: IO ()
main = do
    instructions <- lines <$> readFile "input"
    let result1 = part1 instructions
        result2 = part2 instructions
    putStrLn "Part 1 results"
    print result1
    putStrLn "Part 2 results"
    print result2

part1 :: [String] -> (Int, Int)
part1 = foldl' go (50, 0)
    where
        go :: (Int, Int) -> String -> (Int, Int)
        go (p, c) ('R':xs)
            | pR' `mod` 100 == 0 = (pR', c + 1)
            | otherwise = (pR', c)
            where
                pR' = p + read xs
        go (p, c) ('L':xs)
            | pL' `mod` 100 == 0 = (pL', c + 1)
            | otherwise = (pL', c)
            where
                pL' = p - read xs
        go _ _ = undefined

part2 :: [String] -> (Int, Int)
part2 = foldl' go (50, 0)
    where
        go :: (Int, Int) -> String -> (Int, Int)
        go (p, c) ('R':xs) = (pR', c + binPR' - binP)
            where
                binP = p `div` 100
                binPR' = pR' `div` 100
                pR' = p + read xs
        go (p, c) ('L':xs) = (pL', c + binP - binPL')
            where
                binP = (p - 1) `div` 100
                binPL' = (pL' - 1) `div` 100
                pL' = p - read xs
        go _ _ = undefined
