module Layer where

import Codec.Picture (PixelRGB8)
import Dem (Pixel(..), Slope(..), norm)

data Layer = Water | Grass | Forest | Rocks | Snow deriving(Show, Eq)

data LayerBounds = LayerBounds{
    minAlti :: Float,
    maxAlti :: Float,
    minSlope :: Float,
    maxSlope :: Float
} deriving(Show)

type LayerColor = PixelRGB8

type Config = [(Layer,LayerBounds,LayerColor)]

determineLayer :: Pixel -> Config -> (Layer,LayerBounds,LayerColor)
determineLayer pixel ((layer,bounds,color):xs) | pixel `within` bounds = (layer,bounds,color)
                                           | otherwise = determineLayer pixel xs
  where
    Pixel altitude (Slope slope) `within` LayerBounds minAlti maxAlti minSlope maxSlope =
        altitude >= minAlti && altitude <= maxAlti && let sv = norm slope in (sv >= minSlope && sv <= maxSlope)

layerColor :: Layer -> Config -> LayerColor
layerColor layer ((l,_,color):xs) | layer == l = color
                                  | otherwise = layerColor layer xs
