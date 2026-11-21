module Layer where

data Layer = Water | Grass | Forest | Rocks | Snow deriving(Show, Eq)

data LayerBounds = LayerBounds{
    minAlti :: Float,
    maxAlti :: Float,
    minSlope :: Float,
    maxSlope :: Float
} deriving(Show)

type Config = [(Layer,LayerBounds,LayerColor)]

determineLayer pixel ((layer,bounds,_):xs) | pixel within bounds = layer
                            | otherwise = determineLayer pixel xs
    where (Pixel altitude (Slope slope)) (within) (LayerBounds minAlti maxAlti minSlope maxSlope) = 
        altitude >= minAlti && altitude <= maxAlti && let sv = norm slope in (sv >= minSlope && sv <= maxSlope)

layerColor layer ((l,_,color):xs) | layer == l = color
                                  | otherwise = layerColor layer xs

