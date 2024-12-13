module Inputs.Traffic where
import Huzzy.Base.Sets


-- | Завантаженість дороги, автомобілів на км
type Traffic = Double

low, moderate, high :: MF Traffic
low = down 10 20
moderate = tri 15 25 35
high = up 30 50