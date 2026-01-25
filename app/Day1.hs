module Day1
( part1
) where

import Data.Maybe

data Dial = Dial
  { position :: Int
  , counter :: Int
  } deriving Show

data Rotation = LeftDir Int | RightDir Int deriving Show

plus :: Dial -> Rotation -> Dial
plus (Dial { position, counter }) (LeftDir val) = Dial { position=pos, counter=(counter + zero)}
    where rawPos = (position - val) `mod` 100
          pos = if rawPos < 0 then rawPos + 100 else rawPos
          zero = if pos == 0 then 1 else 0

plus (Dial { position, counter}) (RightDir val) = Dial { position=pos, counter=(counter + zero)}
    where pos = (position + val) `mod` 100
          zero = if pos == 0 then 1 else 0

parseRotation :: String -> Maybe Rotation
parseRotation ('L':numStr) = Just (LeftDir (read numStr))
parseRotation ('R':numStr) = Just (RightDir (read numStr))
parseRotation _ = Nothing 

part1 :: String -> IO Int
part1 filename = do
    contents <- readFile filename
    let ls = lines contents
    let rs = map parseRotation ls
    let rotations = catMaybes rs
    return $ counter $ foldl plus Dial {position=50, counter=0} rotations