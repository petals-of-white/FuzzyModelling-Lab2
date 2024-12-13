module Inputs.RoadSurface where
import           Huzzy.Base.Sets


-- | Коефіцієнт зчеплення з дорогою, [0;1]
type RoadSurface = Double

normal, slippery :: MF RoadSurface
