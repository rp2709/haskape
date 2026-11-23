module Shade where

import Codec.Picture (DynamicImage(..), Pixel8, PixelRGB8(..), generateImage, savePngImage)
import Layer
import Dem

shade :: Float -> PixelRGB8 -> Slope -> Vec -> PixelRGB8
shade strength color (Slope slopeVector) sun = luminescence strength color $ cosPhi sun slopeVector

cosPhi :: Vec -> Vec -> Float
cosPhi v1 v2 = (dotProduct v1 v2) / ((norm v1)*(norm v2))

luminescence :: Float -> PixelRGB8 -> Float -> PixelRGB8
luminescence strength (PixelRGB8 r g b) value = PixelRGB8 (adjust r) (adjust g) (adjust b)
  where
    t = max (-1) (min 1 value) * strength

    adjust :: Pixel8 -> Pixel8
    adjust c
      | t >= 0    = toWord8 (c' + (255 - c') * t)  -- lerp toward white
      | otherwise = toWord8 (c' * (1 + t))         -- lerp toward black
      where c' = fromIntegral c :: Float

    toWord8 :: Float -> Pixel8
    toWord8 = fromIntegral . max 0 . min 255 . (round :: Float -> Int)

colorAndShade :: HaskapeConfiguration -> Pixel -> PixelRGB8
colorAndShade (HaskapeConfiguration sun strength fallbackColor layers) pixel =
    case determineLayer pixel layers of
      Just (Layer _ color False _) -> color  
      Just (Layer _ color _ _) -> shade strength color (pixelSlope pixel) sun
      -- use default color if there is no match in the configuration
      Nothing -> shade strength fallbackColor (pixelSlope pixel) sun

-- ColorAt is a closure containing everything necessary to compute the color of pixel i
saveImage :: FilePath -> Int -> Int -> (Int -> PixelRGB8) -> IO ()
saveImage path w h colorAt = savePngImage path (ImageRGB8 img)
  where
    img = generateImage (\x y -> colorAt (y * w + x)) w h
