module OptimalSpeed where
import           Huzzy.Base.Sets

-- | Оптимальна швидкість, км/год
type OptimalSpeed = Double

veryLow, low, moderate, high, veryHigh :: MF OptimalSpeed
veryLow = down 10 30
low = tri 20 40 60
moderate = tri 50 70 90
high = tri 80 100 120
veryHigh = up 110 130