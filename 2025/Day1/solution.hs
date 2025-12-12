module Main where

main :: IO ()
main = do
    instructions <- lines <$> readFile "input"
    let result = calcZeroes instructions
    print result

calcZeroes :: [String] -> (Int, Int)
calcZeroes = foldl' go (50, 0) 
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
