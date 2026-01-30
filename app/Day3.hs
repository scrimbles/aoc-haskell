module Day3
( part1
, part2
) where

import Data.Ord
import Data.Char
import Data.Maybe

-- TODO: figure out how to remove dependency on custom `maximumBy`
-- custom maximumBy that prefers the first occurrance of an element, rather than the last
maximumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
maximumBy cmp = fromMaybe (errorWithoutStackTrace "maximumBy: empty structure")
  . foldl' max' Nothing
  where
    max' mx y = Just $! case mx of
      Nothing -> y
      Just x -> case cmp x y of
        GT -> x
        EQ -> x -- only required change, in case of eq, prefer existing choice, not new one
        _ -> y

maxIndex :: Ord a => [a] -> (Int, a)
maxIndex = maximumBy (comparing snd) . zip [0..]

maxJoltage :: Int -> [Int] -> [Int]
maxJoltage 0 _ = []
maxJoltage _ [] = []
maxJoltage n digits = (++) [largest] $ maxJoltage digitsRem $ drop (idx+1) digits
    where pair = maxIndex $ take possibles digits
          largest = snd pair
          idx = fst pair
          possibles = (length digits) - digitsRem
          digitsRem = n-1

place :: (Int, Int) -> Int
place (p, d) = d*(10^p)

digify :: [Int] -> Int
digify = (sum . (map place) . (zip [0..]) . reverse)

solve :: Int -> String -> Int
solve n fileContents = do
    let bankStrings = lines fileContents
    let banks = map (map digitToInt) bankStrings
    let joltageSeqs = map (maxJoltage n) banks
    let joltages = map digify joltageSeqs :: [Int]
    sum joltages

part1 :: String -> Int
part1 = solve 2

part2 :: String -> Int
part2 = solve 12