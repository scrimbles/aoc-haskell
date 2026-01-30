import Options.Applicative
import qualified Day1
import qualified Day2
import qualified Day3

data Options where
  Options :: {day :: Integer, part :: Integer} -> Options

sampleParser :: Parser Options
sampleParser = Options
  <$> option auto
      ( long "day"
     <> short 'd'
     <> metavar "INT"
     <> help "Which day to solve for." )
 <*> option auto
     ( long "part"
    <> short 'p'
    <> showDefault
    <> value 1
    <> metavar "INT"
    <> help "Which part [1,2] to solve for for the day.")

getFilename :: Int -> String
getFilename day = "app/puzzles/day_"
  ++ (if day < 10 then "0" ++ show day else show day)
  ++ "/input.txt"

solve' :: (String -> Int) -> Int -> IO ()
solve' f day = (readFile.getFilename) day >>= print.f

solve :: Options -> IO ()
solve (Options {day=1, part=1}) = solve' Day1.part1 1
solve (Options {day=1, part=2}) = solve' Day1.part2 1
solve (Options {day=2, part=1}) = solve' Day2.part1 2
solve (Options {day=2, part=2}) = solve' Day2.part2 2
solve (Options {day=3, part=1}) = solve' Day3.part1 3
solve (Options {day=3, part=2}) = solve' Day3.part2 3
solve _ = putStrLn "Not implemented"

-- 3. Define the program's main action
main :: IO ()
main = do
    parsedOpts <- execParser (info (sampleParser <* helper)
      ( fullDesc
     <> progDesc "Advent of Code solver for 2025."
     <> header "aoc-haskell - AoC Solutions in Haskell - 0.1.0.0" ))
    solve parsedOpts
