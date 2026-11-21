module Shade where

import Codec.Picture hiding(Pixel)
import qualified Data.Vector as V
import Layer
import Dem

data Sun = Sun Vec deriving(Show)

shouldShade :: Layer -> Bool
shouldShade Water = False
shouldShade _ = True

shade :: Layer -> LayerBounds -> LayerColor -> Float -> Slope -> Sun -> LayerColor
shade Water layerBounds color altitude _ _ = 
    let diff = maxAlti layerBounds - altitude in if diff > 10 then color else luminescence color (diff/10)

shade _ _ color _ (Slope slopeVector) (Sun sunVector) = luminescence color $ - cosPhi sunVector slopeVector

cosPhi :: Vec -> Vec -> Float
cosPhi v1 v2 = (dotProduct v1 v2) / ((norm v1)*(norm v2))

-- Maximum strength of the effect. At value = ±1 the color is moved this
-- fraction of the way toward white/black, so it never fully washes out.
luminescenceStrength :: Float
luminescenceStrength = 0.6

luminescence :: LayerColor -> Float -> LayerColor
luminescence (PixelRGB8 r g b) value = PixelRGB8 (adjust r) (adjust g) (adjust b)
  where
    t = max (-1) (min 1 value) * luminescenceStrength

    adjust :: Pixel8 -> Pixel8
    adjust c
      | t >= 0    = toWord8 (c' + (255 - c') * t)  -- lerp toward white
      | otherwise = toWord8 (c' * (1 + t))         -- lerp toward black
      where c' = fromIntegral c :: Float

    toWord8 :: Float -> Pixel8
    toWord8 = fromIntegral . max 0 . min 255 . (round :: Float -> Int)

colorAndShade :: Config -> Pixel -> LayerColor
colorAndShade config pixel =
    let (layer,bounds,color) = determineLayer pixel config
    in shade layer bounds color (pixelAltitude pixel) (pixelSlope pixel) (Sun (Vec 0.25 0.25))


saveImage :: String -> Int -> Int -> [PixelRGB8] -> IO ()
saveImage path w h pixels = savePngImage path (ImageRGB8 img)
  where
    v   = V.fromList pixels
    img = generateImage (\x y -> v V.! (y * w + x)) w h