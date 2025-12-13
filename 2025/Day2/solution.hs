module Main where

import Data.Maybe (Maybe(..), mapMaybe)
import Data.List (foldl')
import Text.Read (readMaybe)

data IDRange = Range {
    minOfIDRange :: !Int
  , maxOfIDRange :: !Int
} deriving Show

main :: IO ()
main = do
    instructions <- mapMaybe parseIDRange . splitOn ',' . takeWhile (/= '\n') <$> readFile "input"
    print $ strictSum $ map part1 instructions
    print $ strictSum $ map part2 instructions

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn delim str = fst splitStr : splitOn delim (snd splitStr)
    where
        splitStr = splitOnce delim str ""

splitOnce :: Char -> String -> String -> (String, String)
splitOnce _ "" accum = (reverse accum, "")
splitOnce delim (x:xs) accum
    | x == delim = (reverse accum, xs)
    | otherwise = splitOnce delim xs $ x:accum

parseIDRange :: String -> Maybe IDRange
parseIDRange "" = Nothing
parseIDRange str = case splitOnce '-' str "" of
  (_,"") -> Nothing
  (x, y) -> do
    intX <- readMaybe x
    intY <- readMaybe y
    pure Range { minOfIDRange=intX, maxOfIDRange=intY }

part1 :: IDRange -> Int
part1 Range { minOfIDRange = min , maxOfIDRange = max } = strictSum $ filter p [min..max]
    where
        p i
            | even lenStr = a == b
            | otherwise = False
            where
                (a, b) = splitAt (lenStr `div` 2) str
                str = show i
                lenStr = length str

strictSum :: Integral a => [a] -> a
strictSum = foldl' (+) 0

part2 :: IDRange -> Int
part2 Range { minOfIDRange = min , maxOfIDRange = max } =  strictSum $ filter go [min..max]
    where
        go x = p (length $ show x) x

p 1 intStr = False
p i intStr = concat (replicate i $ take (length str `div` i) str) == str || p (i-1) intStr
    where
        str = show intStr
