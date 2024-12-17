module Parsing where
import           GraphicOutput
import           Huzzy.Base.Sets
import           Huzzy.TypeOne.Sets
import           Huzzy.TypeOne.Systems
import           Inputs                (CrispInput (CrispInput))
import           Inputs.RoadType       (RoadType (..))
import           OptimalSpeed          (OptimalSpeed)
import           Options.Applicative
import           System                (fuzzyOptimalSpeed)
import           Text.Read             (readMaybe)

data Mode = Interactive | Immediate CrispInput FilePath

processInput :: CrispInput -> FilePath -> IO ()
processInput input saveLoc = do
    print input
    putStrLn "Дефазифікація. Метод: centroid"
    putStrLn ("Чітке значення оптимальної швидкості: " ++ show crispSpeed ++ " км/год")
    outputChart
        [(speed, f speed) | speed <- [0..150 :: OptimalSpeed]]
        (crispSpeed, f crispSpeed)
        saveLoc
    where
        optimalSpeed@(MF f) = fuzzyOptimalSpeed input
        crispSpeed = centroid (discT1 [0..150] optimalSpeed)

run :: Mode -> IO ()
run (Immediate input savePath) = processInput input savePath

run Interactive = do
    input <- (CrispInput <$> askRoadType) <*> askInput "Видимість (м)" <*> askInput "Освітленість (лк)"
        <*> askInput "Коефіцієнт зчеплення [0;1]" <*> askInput "Завантаженість дороги (авто/км)"
        <*> askInput "Інтенсивність опадів, мм/год" <*> askInput "Куь нахилу дороги [-180; 180]"

    saveLocation <- askString "Куди зберегти графік? "

    processInput input saveLocation

parser :: ParserInfo Mode
parser = info (modeParser <**> helper) fullDesc

modeParser :: Parser Mode
modeParser = immediateParser <|> interactiveParser

immediateParser :: Parser Mode
immediateParser = Immediate <$> (CrispInput <$>
    option auto (long "roadType")
    <*> option auto (long "maxVisibility")
    <*> option auto (long "illuminance")
    <*> option auto (long "roadSurface")
    <*> option auto (long "traffic")
    <*> option auto (long "precipationRate")
    <*> option auto (long "roadSlope")) <*> strOption (long "output" <> short 'o')

interactiveParser :: Parser Mode
interactiveParser = flag' Interactive (long "interactive" <> short 'i')


askRoadType :: IO RoadType
askRoadType = do
    putStrLn "тип дороги: (0 - пішохідна зона, 1 - населений пункт, 2 - поза населеним пунктом, 3 - автомагістраль) "
    inp <- getLine

    case (readMaybe inp :: Maybe Int) of
        Just 0  -> return PedestrianZone
        Just 1  -> return Settlement
        Just 2  -> return OutOfSettlement
        Just 3  -> return Highway
        Just _  -> putStrLn "Некоректне значення!" >> askRoadType
        Nothing -> putStrLn "Некоректне значення!" >> askRoadType


askString :: String -> IO String
askString prompt = do
    putStrLn $ prompt ++ ": "
    getLine
askInput :: Read a => String -> IO a
askInput prompt = do
    putStrLn $ prompt ++ ": "
    inp <- getLine
    case readMaybe inp of
        Just something -> return something
        Nothing        -> putStrLn "Некоректне значення!" >> askInput prompt
