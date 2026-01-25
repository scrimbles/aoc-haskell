import Options.Applicative
import Data.Monoid (mconcat)
import qualified Day1

-- 1. Define a data type to hold the parsed arguments
data Options = Options
  { day   :: Integer
  , part  :: Integer
    -- flags and other options can be added here
  }

-- 2. Define the parser for your options
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

solve :: Options -> IO ()
solve (Options {day = 1, part = 1}) = do
  solution <- Day1.part1 "app/puzzles/day_01/input.txt"
  print solution
solve _ = putStrLn "Not implemented"

-- 3. Define the program's main action
main :: IO ()
main = do
    parsedOpts <- execParser (info (sampleParser <* helper)
      ( fullDesc
     <> progDesc "Advent of Code solver for 2025."
     <> header "aoc-haskell - AoC Solutions in Haskell - 0.1.0.0" ))
    solve parsedOpts
