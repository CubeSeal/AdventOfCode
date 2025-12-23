module Main where

import Data.Maybe (Maybe(..), mapMaybe, fromMaybe, isJust)
import Data.Char (digitToInt)
import Data.Traversable (for)
import Data.List (foldl', sortOn, uncons, find)
import Text.Read (readMaybe)

data Range a = Range a a

instance Show a => Show (Range a) where
  show (Range x y) = show x ++ "-" ++ show y

type DataBase = [Range Int]
type QueryValues = [Int]

main :: IO ()
main = do
  (rawDB, rawQuery) <- break (== "") . lines <$> readFile "input"
  let
    parsedDB = cleanDB $ sortDB $ fromMaybe undefined $ parseMinMaxList rawDB
    parsedQuery = map read (drop 1 rawQuery)
  print $ part1 parsedDB parsedQuery
  print $ part2 parsedDB

-- Similar to span and break, but just matches with (==) delim and consumes delimiter
splitOnce :: Char -> String -> (String, String)
splitOnce delim str = go delim str ""
  where
  go _ "" accum = (reverse accum, "")
  go delim (x:xs) accum
    | x == delim = (reverse accum, xs)
    | otherwise = go delim xs $ x:accum

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn delim str = fst splitStr : splitOn delim (snd splitStr)
  where
      splitStr = splitOnce delim str

parseMinMaxList :: [String] -> Maybe [Range Int]
parseMinMaxList [] = Nothing
parseMinMaxList strings = for strings parseString
  where
    parseString string = do
      let
        (rawMin, rawMax) = splitOnce '-' string
      intMin <- readMaybe rawMin
      intMax <- readMaybe rawMax
      return $ Range intMin intMax

sortDB :: Ord a => [Range a] -> [Range a]
sortDB = sortOn f
  where
    f (Range x _) = x

cleanDB :: Ord a => [Range a] -> [Range a]
cleanDB [] = []
cleanDB [x] = [x]
cleanDB (Range l1 h1: Range l2 h2 : ls)
  | h1 >= l2
    || l1 == l2 
    || h1 == h2 = cleanDB (Range l1 (max h1 h2): ls)
  | otherwise = Range l1 h1 : cleanDB (Range l2 h2 : ls)

part1 :: DataBase -> QueryValues -> Int
part1 db values = sum $ map (fromEnum . query) values
  where
    query v = isJust $ find (f v) db 
    f v (Range x y) = (x <= v) && (v <= y)

part2 :: DataBase -> Int
part2 db = sum $ map f db
  where
    f (Range x y) = y - x + 1
