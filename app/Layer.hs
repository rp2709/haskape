module Layer where

import Codec.Picture (PixelRGB8)
import Dem (Pixel(..), Slope(..), norm, Vec)

data Layer = Layer String PixelRGB8 Bool LayerBounds deriving(Show, Eq)

data LayerBounds = LayerBounds{
    minAlti :: Float,
    maxAlti :: Float,
    minSlope :: Float,
    maxSlope :: Float
} deriving(Show, Eq)

-- sunDirection, shadeStrength, layers
data HaskapeConfiguration = HaskapeConfiguration Vec Float PixelRGB8 [Layer] deriving (Show, Eq)

determineLayer :: Pixel -> [Layer] -> Maybe Layer
determineLayer _ [] = Nothing
determineLayer pixel (layer@(Layer _ _ _ bounds):xs)
    | pixel `within` bounds = Just layer
    | otherwise             = determineLayer pixel xs
  where
    Pixel altitude (Slope slope) `within` b =
        altitude >= minAlti b && altitude <= maxAlti b && let sv = norm slope in (sv >= minSlope b && sv <= maxSlope b)
