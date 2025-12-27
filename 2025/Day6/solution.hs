module Main where

import Data.Char (isSpace)
import Data.List (transpose, foldl', unsnoc)
import Data.Maybe (fromMaybe)

data OperationHolder = OperationHolder [String] String

main :: IO ()
main = do
  fileStr <- readFile "input"
  print $ part1 fileStr
  print $ part2 fileStr

part1 :: String -> Int
part1 rawFileStr = foldl' go 0 parsedStr
  where
    parsedStr = transpose . map words . lines $ rawFileStr
    go accum numOrOp =
      let
        (nums, op) = fromMaybe undefined $ unsnoc numOrOp
        parsedNums = map read nums
      in
        case op of
          "+" -> sum parsedNums + accum
          "*" -> product parsedNums + accum

part2 :: String -> Int
part2 rawFileStr = foldl' go 0 parsedOperations
  where
    parsedOperations = parseOperations $ lines rawFileStr
    go cumSum (OperationHolder x "*") = cumSum + product (map read x)
    go cumSum (OperationHolder x "+") = cumSum + sum (map read x)

parseOperations :: [String] -> [OperationHolder]
parseOperations linesStr = zipWith OperationHolder filteredNums $ words ops
  where
    (nums, ops) = fromMaybe undefined $ unsnoc linesStr
    parsedNums = transpose nums
    filteredNums = func [] parsedNums
    func accum [] = [reverse accum]
    func accum (x:xs)
      | all isSpace x = reverse accum : func [] xs
      | otherwise = func (x:accum) xs
