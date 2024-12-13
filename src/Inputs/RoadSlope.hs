module Inputs.RoadSlope where
import           Huzzy.Base.Sets

-- | Кут нахилу дороги. градуси [-180; 180]
type RoadSlope = Double

steepAscent, smoothAscent, flat, smoothDescent, steepDescent :: MF RoadSlope
steepAscent = trap 15 20 30 40
smoothAscent = tri 5 10 15
flat = tri (-5) 0 5
smoothDescent = tri (-15) (-10) (-5)
steepDescent = trap (-40) (-30) (-20) (-15)
