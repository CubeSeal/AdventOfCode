module Main where

import Data.Char (digitToInt)
import Data.List (foldl')

type IndexValue = (Int, Int)

main :: IO ()
main = do
  input <- map (map digitToInt) . lines <$> readFile "input"
  print $ sum $ map part1 input
  print $ sum $ map (part2 2) input
  print $ sum $ map (part2 12) input

whichMax :: [Int] -> IndexValue
whichMax = snd . foldl' go (0, (0, 0))
   where
    go :: (Int, IndexValue) -> Int -> (Int, IndexValue)
    go (i, (currMaxIndex, currMax)) value = if value > currMax
      then (i+1, (i, value))
      else (i+1, (currMaxIndex, currMax))

part1 :: [Int] -> Int
part1 bank = (10 * snd firstMax) + snd secondMax
  where
    firstMax = whichMax $ take (length bank - 1) bank
    secondMax = whichMax $ drop (fst firstMax + 1) bank

part2 :: Int -> [Int] -> Int
part2 0 _ = 0
part2 digits bank = (10^nextIterDigits * snd digitMax) + part2 nextIterDigits (drop (fst digitMax + 1) bank)
  where
    digitMax = whichMax $ take (length bank - nextIterDigits) bank
    nextIterDigits = digits - 1
