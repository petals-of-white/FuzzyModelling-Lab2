module Main where
import           Huzzy.Base.Sets
import           Inputs              (CrispInput (CrispInput))
import           Options.Applicative
import           System              

data Mode = Interactive | Immediate CrispInput

immediateParser :: Parser Mode
immediateParser = Immediate <$> (CrispInput <$>
    option auto (long "roadType")
    <*> option auto (long "maxVisibility")
    <*> option auto (long "illuminance")
    <*> option auto (long "roadSurface")
    <*> option auto (long "traffic")
    <*> option auto (long "precipationRate")
    <*> option auto (long "roadSlope"))

interactiveParser :: Parser Mode
interactiveParser = flag' Interactive (long "interactive" <> short 'i')

modeParser :: Parser Mode
modeParser = immediateParser <|> interactiveParser

parser :: ParserInfo Mode
parser = info (modeParser <**> helper) fullDesc

run :: Mode -> IO ()
run (Immediate input) = print $ map f [0..150]
    where (MF f) = fuzzyOptimalSpeed input
run Interactive = putStrLn "Interactive mode"
-- run :: Mode -> IO ()
-- run (Immediate input) = print $ map (\(MF f) -> map f[0..100]) rules
--     where rules = makeRules input
-- run Interactive = putStrLn "Interactive mode"

main :: IO ()
main = execParser parser >>= run