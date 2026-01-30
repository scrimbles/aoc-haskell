module Day2
( part1
, part2
) where

import Data.List.Split

doubleSeq :: String -> Bool
doubleSeq s
    | (length s) `mod` 2 /= 0 = False
    | otherwise = invalid' s ((length s) `div` 2)

invalid :: String -> Bool
invalid s = foldl (||) False $ map (invalid' s) [1..l]
    where l = (length s) `div` 2

invalid' :: String -> Int -> Bool
invalid' s n
    | (length s) `mod` n /= 0 = False
    | otherwise = case chunks of
                      (firstChunk:_) -> all (== firstChunk) chunks
                      [] -> False
        where chunks = chunksOf n s

getRangePairs :: String -> [[Int]]
getRangePairs contents = map (map read) $ map (splitOn "-") $ splitOn "," contents

range :: [Int] -> [Int]
range (start:end:[]) = [start..end]
range _ = []

getInvalidIds :: [[Int]] -> [Int]
getInvalidIds rangePairs = concat $ map range rangePairs

solve :: (String -> Bool) -> String -> Int
solve f input = sum $ filter (f.show) $ getInvalidIds $ getRangePairs input

part1 :: String -> Int
part1 = solve doubleSeq

part2 :: String -> Int
part2 = solve invalid