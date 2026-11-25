module Shade where

import Codec.Picture (DynamicImage(..), Pixel8, PixelRGB8(..), generateImage, savePngImage)
import Layer
import Dem
import qualified Data.Vector as V

shade :: Float -> PixelRGB8 -> Slope -> Vec -> PixelRGB8
shade strength color (Slope slopeVector) sun = luminescence strength color $ cosPhi sun slopeVector

cosPhi :: Vec -> Vec -> Float
cosPhi v1 v2
  | norm v1 == 0 || norm v2 == 0 = 0
  | otherwise = (dotProduct v1 v2) / ((norm v1)*(norm v2))

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

-- returns the scaled down image and its new width and height
scaleDown :: V.Vector PixelRGB8 -> (Int,Int) -> Int -> (V.Vector PixelRGB8,(Int,Int))
scaleDown src (width,height) desiredWidth
  | desiredWidth <= 0 || desiredWidth >= width = (src,(width,height))
  | otherwise = (V.generate (newW * newH) _pixelAt, (newW,newH))
  where
    newW = desiredWidth
    newH = max 1 (height * desiredWidth `div` width)   -- keep aspect ratio

    _pixelAt i =
      let (y,x) = i `divMod` newW
          -- source block covered by destination pixel (x,y)
          x0 = x * width `div` newW
          x1 = max (x0 + 1) ((x + 1) * width `div` newW)
          y0 = y * height `div` newH
          y1 = max (y0 + 1) ((y + 1) * height `div` newH)

          (sr,sg,sb) = foldl' add (0,0,0)
            [ src V.! (sy * width + sx) | sy <- [y0 .. y1 - 1], sx <- [x0 .. x1 - 1] ]
          n = (x1 - x0) * (y1 - y0)
      in PixelRGB8 (fromIntegral (sr `div` n))
                   (fromIntegral (sg `div` n))
                   (fromIntegral (sb `div` n))

    add (r,g,b) (PixelRGB8 r' g' b') =
      (r + fromIntegral r', g + fromIntegral g', b + fromIntegral b') :: (Int,Int,Int)
