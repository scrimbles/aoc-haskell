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
    | otherwise = foldl (&&) True $ map ((==) firstChunk) chunks
        where chunks = chunksOf n s
              firstChunk = head chunks

solve :: (String -> Bool) -> String -> Int
solve f contents = do
    let rangeStrs = splitOn "," contents
    let rangePairs = map (map read) $ map (splitOn "-") rangeStrs :: [[Int]]
    let ranges = map (\p -> [p!!0..p!!1]) rangePairs
    let invalidIds = filter (f.show) $ concat ranges
    sum invalidIds

part1 :: String -> Int
part1 = solve doubleSeq

part2 :: String -> Int
part2 = solve invalid