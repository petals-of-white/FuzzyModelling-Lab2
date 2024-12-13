module Inputs.Precipitation where
import           Huzzy.Base.Sets

-- | Швидкість опадів, мм/год
type Precipation = Double

no, light, moderate, heavy :: MF Precipation
no = down 0 10
light = tri 5 15 25
moderate = tri 10 20 30
heavy = up 20 40