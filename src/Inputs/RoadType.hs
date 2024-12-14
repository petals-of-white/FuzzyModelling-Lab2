{-# LANGUAGE LambdaCase #-}
module Inputs.RoadType where

import           Huzzy.Base.Sets

data RoadType = PedestrianZone | Settlement | OutOfSettlement | Highway deriving (Show, Read)

pedestrianZone, settlement, outOfSettlement, highway :: MF RoadType

pedestrianZone = MF (\case PedestrianZone -> 1; _ -> 0)
settlement = MF (\case Settlement -> 1; _ -> 0)
outOfSettlement = MF (\case OutOfSettlement -> 1; _ -> 0)
highway = MF (\case Highway -> 1; _ -> 0)
