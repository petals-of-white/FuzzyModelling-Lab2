module Inputs.RoadSlope where
import Huzzy.Base.Sets

-- | Кут нахилу дороги. градуси [-180; 180]
type RoadSlope = Int

steepAscent, smoothAscent, flat, smoothDescent, steepDescent :: MF RoadSlope