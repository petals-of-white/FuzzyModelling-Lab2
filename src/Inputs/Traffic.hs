module Inputs.Traffic where
import Huzzy.Base.Sets


-- | Завантаженість дороги, автомобілів на км
type Traffic = Double

low, moderate, high :: MF Traffic