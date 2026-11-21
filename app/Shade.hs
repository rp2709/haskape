module Shade where

import Codec.Picture

data Vec = Vec Float Float deriving (Show)
data Slope = Slope Vec deriving(Show)
data Sun = Sun Vec deriving(Show)
type LayerColor PixelRGB8

shouldShade Water = false
shouldshade _ = true

shade Water (LayerBounds _ maxAlti _ _) color altitude _ _ = 
    let diff = maxAlti - altitude in if diff > 10 then color else luminescence color (diff/10)

shade _ _ color _ (Slope slopeVector) (Sun sunVector) = luminescence color $ - cosPhi sunVector slopeVector

dotProduct (Vec x1 y1) (Vec x2 y2) = x1*x2 + y1*y2
norm (Vec x y) = sqrt $ x*x + y*y

cosPhi v1 v2 = (dotProduct v1 v2) / ((norm v1) (norm v2))

luminescence (LayerColor r g b) value = 