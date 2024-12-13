module Inputs.Illuminance where
import           Huzzy.Base.Sets

-- | Освітленість, люкс
type Illuminance = Double

good, ok, bad :: MF Illuminance
bad = down 5 10
ok = trap 5 10 30 50
good = up 30 50
