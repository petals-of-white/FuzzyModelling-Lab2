module Main where
import           Options.Applicative
import           Parsing

main :: IO ()
main = execParser parser >>= run
