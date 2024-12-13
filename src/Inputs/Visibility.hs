module Inputs.Visibility where
import           Huzzy.Base.Sets as Huzzy

-- | Максимальна видимість, метрів
type Visibility = Double

low, moderate, high :: MF Visibility
low = down 50 10
moderate = trap 50 100 300 500
high = up 300 500