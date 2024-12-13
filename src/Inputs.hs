module Inputs where

import           Inputs.Illuminance
import           Inputs.Precipitation
import           Inputs.RoadSlope
import           Inputs.RoadSurface
import           Inputs.RoadType
import           Inputs.Traffic
import           Inputs.Visibility

-- | Усі вхідні змінні
data CrispInput = CrispInput {
    roadType        :: RoadType,
    maxVisibility   :: Visibility,
    illuminance     :: Illuminance,
    roadSurface     :: RoadSurface,
    traffic         :: Traffic,
    precipationRate :: Precipation,
    roadSlope       :: RoadSlope}
