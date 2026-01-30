module Day1
( part1
, part2
) where

import Data.Maybe

data Dial = Dial
  { position :: Int
  , counter :: Int
  } deriving Show

data Rotation = LeftDir Int | RightDir Int deriving Show

isZero :: Int -> Int
isZero x
  | x == 0 = 1
  | otherwise = 0

type ZeroCounter = Dial -> Rotation -> Int
plus :: ZeroCounter -> Dial -> Rotation -> Dial
plus z dial rot =
  Dial { position=newPos, counter=newCount } 
  where newPos = (plus' dial rot)
        newCount = (counter dial) + (z dial rot)

plus' :: Dial -> Rotation -> Int
plus' (Dial {position}) (LeftDir dist) =
  if pos < 0 then pos + 100 else pos
  where pos = (position - dist) `mod` 100
plus' (Dial {position}) (RightDir dist) = (position + dist) `mod` 100

zeroDetector :: ZeroCounter
zeroDetector d r = isZero $ plus' d r

zeroCounter :: ZeroCounter
zeroCounter dial rot = (loops rot) + (passesZero dial rot)

passesZero :: Dial -> Rotation -> Int
passesZero (Dial {position}) (LeftDir dist) = if position /= 0 && position <= (dist `mod` 100) then 1 else 0
passesZero (Dial {position}) (RightDir dist) = if position /= 0 && (100 - position) <= (dist `mod` 100) then 1 else 0

loops ::Rotation -> Int
loops (LeftDir n) = n `div` 100
loops (RightDir n) = n `div` 100


parseRotation :: String -> Maybe Rotation
parseRotation ('L':numStr) = Just (LeftDir (read numStr))
parseRotation ('R':numStr) = Just (RightDir (read numStr))
parseRotation _ = Nothing 

solve :: (Dial -> Rotation -> Dial) -> String -> Int
solve f contents = counter $ foldl f Dial {position=50, counter=0} $ catMaybes $ map parseRotation $ lines contents

part1 :: String -> Int
part1 = solve (plus zeroDetector)

part2 :: String -> Int
part2 = solve (plus zeroCounter)