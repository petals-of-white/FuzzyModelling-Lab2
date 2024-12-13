module Inputs where

import           Huzzy.Base.Sets
import           Inputs.Illuminance
import           Inputs.RoadType

-- | Усі вхідні змінні
data CrispInput = CrispInput {
    roadType        :: RoadType,
    maxVisibility   :: MaxVisibility,
    illuminance     :: Illuminance,
    gripCoefficient :: GripCoefficient,
    carsPerKm       :: CarsPerKm,
    precipationRate :: PrecipationRate,
    roadSlope       :: RoadSlope}
