module System where
import           Huzzy.Base.Sets    hiding (FSet (is))
import           Inputs

import           Huzzy.Base.Systems
import qualified Inputs.Illuminance as Illuminance
import           Inputs.RoadType    as RoadType
import           OptimalSpeed             

apply :: MF a -> a -> Double
apply (MF f) = f

is :: a -> MF a -> Double
is e (MF f) = f e

makeRules :: CrispInput -> [MF OptimalSpeed]
makeRules CrispInput {
    roadType,
    maxVisibility,
    illuminance,
    roadSurface,
    traffic,
    precipationRate,
    roadSlope
} = 
    [
        -- (roadType `is` RoadType.highway ?&& roadSurface `is` Illuminance.ok) `weight` 0.6 =|> OptimalSpeed.high
    ]
