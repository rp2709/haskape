module Shade where

import Codec.Picture (DynamicImage(..), Pixel8, PixelRGB8(..), generateImage, savePngImage)
import Layer
import Dem

sunDirection :: Vec
sunDirection = Vec (-0.25) (-0.25)

shade :: Layer -> LayerBounds -> LayerColor -> Float -> Slope -> Vec -> LayerColor
shade Water layerBounds color altitude _ _ =
    let diff = maxAlti layerBounds - altitude in if diff > 10 then color else luminescence color (diff/10)
shade _ _ color _ (Slope slopeVector) _ = luminescence color $ - cosPhi sunDirection slopeVector

cosPhi :: Vec -> Vec -> Float
cosPhi v1 v2 = (dotProduct v1 v2) / ((norm v1)*(norm v2))

-- | Maximum strength of the effect. At value = ±1 the color is moved this
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

fallbackColor :: LayerColor
fallbackColor = PixelRGB8 128 128 128

colorAndShade :: Config -> Pixel -> LayerColor
colorAndShade config pixel =
    case determineLayer pixel config of
      Just (layer,bounds,color) -> shade layer bounds color (pixelAltitude pixel) (pixelSlope pixel) sunDirection
      -- defaults to gray rocks  if there is no match in the configuration
      Nothing -> shade Rocks (LayerBounds 0 0 0 0) fallbackColor (pixelAltitude pixel) (pixelSlope pixel) sunDirection

-- ColorAt is a closure containing everything necessary to compute the color of pixel i
saveImage :: FilePath -> Int -> Int -> (Int -> LayerColor) -> IO ()
saveImage path w h colorAt = savePngImage path (ImageRGB8 img)
  where
    img = generateImage (\x y -> colorAt (y * w + x)) w h
