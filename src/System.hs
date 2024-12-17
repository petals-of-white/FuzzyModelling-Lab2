module System where
import           Huzzy.Base.Sets      hiding (FSet (is))
import           Huzzy.Base.Systems
import           Inputs
import qualified Inputs.Illuminance   as Illuminance
import qualified Inputs.Precipitation as Precipation
import qualified Inputs.RoadSlope     as RoadSlope
import qualified Inputs.RoadSurface   as RoadSurface
import           Inputs.RoadType      as RoadType
import qualified Inputs.Traffic       as Traffic
import qualified Inputs.Visibility    as Visibility
import           OptimalSpeed

-- | Звичайне застосування функції належності
is :: a -> MF a -> Double
is e (MF f) = f e

-- | Акумуляція списку правил за допомогою бінарної операції.
ruleBase :: Fuzzy a => (a -> a -> a) -> [a] -> a
ruleBase = foldr1

-- | Етап акумуляції - це просто всіх правил за допомогою нечіткого АБО
accumulate :: Fuzzy a => [a] -> a
accumulate = ruleBase (?||)

fuzzyOptimalSpeed :: CrispInput -> MF OptimalSpeed
fuzzyOptimalSpeed = accumulate . makeRules

-- | Правила використовують prod активацію
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
        (roadType `is` RoadType.pedestrianZone) =*> OptimalSpeed.veryLow,

        (roadType `is` RoadType.settlement ?&& maxVisibility `is` Visibility.low) =*> OptimalSpeed.veryLow,

        (roadType `is` RoadType.settlement ?&& traffic `is` Traffic.high) =*> OptimalSpeed.veryLow,

        (roadType `is` RoadType.settlement ?&&
            roadSurface `is` RoadSurface.slippery ?&& maxVisibility `is` Visibility.low) =*> OptimalSpeed.veryLow,

        (roadType `is` RoadType.settlement ?&& precipationRate `is` Precipation.heavy
            ?&& illuminance `is` Illuminance.bad) =*> OptimalSpeed.veryLow,


        (roadType `is` RoadType.settlement ?&& roadSurface `is` RoadSurface.normal
            ?&& roadSlope `is` RoadSlope.flat) =*> OptimalSpeed.low,

        (roadType `is` RoadType.outOfSettlement ?&& roadSlope `is` RoadSlope.steepDescent
        ?&& (roadSurface `is` RoadSurface.slippery
                ?|| (illuminance  `is` Illuminance.bad ?&& maxVisibility `is` Visibility.low)))
                =*> OptimalSpeed.veryLow,


        (roadType `is` RoadType.outOfSettlement ?&& illuminance `is` Illuminance.ok
        ?&& precipationRate `is` Precipation.moderate ?&& maxVisibility `is` Visibility.moderate)
            =*> OptimalSpeed.low,

        (roadType `is` RoadType.outOfSettlement ?&& precipationRate `is` Precipation.light
            ?&& traffic `is` Traffic.moderate) =*> OptimalSpeed.moderate,

        (roadType `is` RoadType.outOfSettlement ?&& roadSurface `is` RoadSurface.normal) =*> OptimalSpeed.high,


        (roadType `is` RoadType.highway ?&& roadSlope `is` RoadSlope.steepDescent

        ?&& (precipationRate `is` Precipation.heavy ?|| traffic `is` Traffic.high)) =*> OptimalSpeed.veryLow,


        (roadType `is` RoadType.highway ?&& roadSlope `is` RoadSlope.smoothDescent
        ?&& roadSurface `is` RoadSurface.slippery
        ?&& maxVisibility `is` Visibility.low) =*> OptimalSpeed.low,



        (roadType `is` RoadType.highway ?&& roadSlope `is` RoadSlope.steepAscent
            ?&& roadSurface `is` RoadSurface.slippery) =*> OptimalSpeed.moderate,

        (roadType `is` RoadType.highway
        ?&& illuminance `is` Illuminance.ok ?&& precipationRate `is` Precipation.light) =*> OptimalSpeed.high,



        (roadType `is` RoadType.highway ?&& precipationRate `is` Precipation.no
        ?&& roadSlope `is` RoadSlope.flat ?&& roadSurface `is` RoadSurface.normal
        ?&& maxVisibility `is` Visibility.high) =*> OptimalSpeed.veryHigh
    ]
