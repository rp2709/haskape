module Dem where

import Codec.Picture hiding (Pixel)
import qualified Codec.Picture as JP
import Data.List (transpose)

type Grid = [[Float]]

-- | A 2D vector, used for slope gradients (and, later, the sun direction).
data Vec = Vec Float Float deriving (Show, Eq)

dotProduct :: Vec -> Vec -> Float
dotProduct (Vec x1 y1) (Vec x2 y2) = x1*x2 + y1*y2
norm :: Vec -> Float
norm (Vec x y) = sqrt $ x*x + y*y

-- | The slope at a pixel, expressed as a gradient vector.
data Slope = Slope Vec deriving (Show, Eq)

-- | A terrain pixel: the altitude at a grid cell together with its slope.
data Pixel = Pixel
  { pixelAltitude :: Float
  , pixelSlope    :: Slope
  } deriving (Show, Eq)

loadGrid :: FilePath -> IO (Either String Grid)
loadGrid path = do
    img <- readPng path
    case img of
        Left msg -> return $ Left msg
        Right dynamicImage -> return $ toGrid dynamicImage

toGrid :: DynamicImage -> Either String Grid
toGrid (ImageY8  img) = Right (imageToGrid img)
toGrid (ImageY16 img) = Right (imageToGrid img)
toGrid _              = Left "Unsupported format: expected 8 or 16 bit greyscale PNG"

imageToGrid :: (JP.Pixel a, Integral a) => Image a -> Grid
imageToGrid img =
  [ [ fromIntegral (pixelAt img x y) | x <- [0 .. imageWidth img - 1] ] | y <- [0 .. imageHeight img - 1] ]

scaleWith :: Float -> Float -> Grid -> Grid
scaleWith altMin altMax img = map (map scale) img
  where
    values     = concat img -- flattens the image to then extact min and max values
    imgMin     = minimum values
    imgMax     = maximum values
    pixelRange = imgMax - imgMin
    factor     = (altMax - altMin) / pixelRange
    scale p
      | pixelRange == 0 = altMin -- a flat image has no altitude variation
      | otherwise       = (p - imgMin) * factor + altMin

-- Differences between consecutive elements. The last element reuses the previous slope.
-- A row of length 0 or 1 has no neighbour: 
diffsRepeatLast :: [Float] -> [Float]
diffsRepeatLast xs =
  case zipWith (-) (drop 1 xs) xs of
    [] -> map (const 0) xs
    ds -> ds ++ [last ds]

-- Pass 1: slopeX = value[x+1] - value[x], per row
slopeX :: Grid -> Grid
slopeX = map diffsRepeatLast

-- Pass 2: slopeY = value[y+1] - value[y], per column.
-- A column is a row of the transpose, so reuse the same function.
slopeY :: Grid -> Grid
slopeY = transpose . map diffsRepeatLast . transpose

-- Zip both components together: one (dx, dy) per pixel, same shape as the image
slopes :: Grid -> [[(Float,Float)]]
slopes g = zipWith zip (slopeX g) (slopeY g)

-- | Flatten an altitude grid and zip each altitude with the slope at the same
--   position, producing one 'Pixel' per image pixel (row-major order).
pixels :: Grid -> [Pixel]
pixels altitudes = zipWith Pixel (concat altitudes) (map toSlope (concat (slopes altitudes)))
  where
    toSlope (dx, dy) = Slope (Vec dx dy)

-- | Whole pipeline: open the image with JuicyPixels, rescale the raw pixel
--   values to the requested altitude range, then produce the flat 'Pixel' list.
loadPixels :: FilePath -> Float -> Float -> IO (Either String [Pixel])
loadPixels path altMin altMax = do
    result <- loadGrid path
    return (fmap (pixels . scaleWith altMin altMax) result)
