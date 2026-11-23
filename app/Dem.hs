module Dem where

import Codec.Picture hiding (Pixel, pixelAt)
import qualified Data.Vector.Storable as S
import Data.Word (Word8, Word16)

-- | A DEM is kept in the compact storable vector JuicyPixels gives us.
--   Scaling parameters are stored instead of a second, boxed grid: samples
--   are converted to altitudes only when a pixel is actually needed.
data Grid
  = Grid8  Int Int (S.Vector Word8)  Float Float
  | Grid16 Int Int (S.Vector Word16) Float Float

gridWidth :: Grid -> Int
gridWidth  (Grid8 w _ _ _ _) = w
gridWidth  (Grid16 w _ _ _ _) = w

gridHeight :: Grid -> Int
gridHeight  (Grid8 _ h _ _ _) = h
gridHeight  (Grid16 _ h _ _ _) = h

gridCount :: Grid -> Int
gridCount g = gridWidth g * gridHeight g

-- | Raw sample at a row-major index, mapped into the requested altitude range.
sampleAt :: Grid -> Int -> Float
sampleAt (Grid8  _ _ vector offset factor) i = fromIntegral (vector S.! i) * factor + offset
sampleAt (Grid16 _ _ vector offset factor) i = fromIntegral (vector S.! i) * factor + offset

-- | A 2D vector, used for slope gradients (and, later, the sun direction).
data Vec = Vec Float Float deriving (Show, Eq)

dotProduct :: Vec -> Vec -> Float
dotProduct (Vec x1 y1) (Vec x2 y2) = x1*x2 + y1*y2

norm :: Vec -> Float
norm (Vec x y) = sqrt $ x*x + y*y

-- | The slope at a pixel, expressed as a gradient vector.
data Slope = Slope !Vec deriving (Show, Eq)

-- | A terrain pixel: the altitude at a grid cell together with its slope.
data Pixel = Pixel
  { pixelAltitude :: Float
  , pixelSlope    :: Slope
  } deriving (Show, Eq)

loadGrid :: FilePath -> IO (Either String Grid)
loadGrid path = do
    img <- readPng path
    return $ case img of
        Left msg          -> Left msg
        Right dynamicImage -> toGrid dynamicImage

toGrid :: DynamicImage -> Either String Grid
toGrid (ImageY8 img)  = Right $ Grid8  (imageWidth img) (imageHeight img) (imageData img) 0 1
toGrid (ImageY16 img) = Right $ Grid16 (imageWidth img) (imageHeight img) (imageData img) 0 1
toGrid _              = Left "Unsupported format: expected 8 or 16 bit greyscale PNG"

-- | Replace the scaling parameters.
scaleWith :: Float -> Float -> Grid -> Grid
scaleWith altMin altMax (Grid8 w h raw _ _) =
    let (offset, factor) = scaleParams altMin altMax (fromIntegral (S.minimum raw)) (fromIntegral (S.maximum raw))
    in Grid8 w h raw offset factor
scaleWith altMin altMax (Grid16 w h raw _ _) =
    let (offset, factor) = scaleParams altMin altMax (fromIntegral (S.minimum raw)) (fromIntegral (S.maximum raw))
    in Grid16 w h raw offset factor

-- | @sample * factor + offset@ maps raw samples to the altitude range.
--   For a flat image every sample becomes @altMin@.
scaleParams :: Float -> Float -> Float -> Float -> (Float, Float)
scaleParams altMin altMax lo hi
  | range == 0 = (altMin, 0)
  | otherwise  = (altMin - lo * factor, factor)
  where
    range  = hi - lo
    factor = (altMax - altMin) / range

slopeAt :: Grid -> Int -> (Float, Float)
slopeAt g i = (dx, dy)
  where
    w    = gridWidth g
    h    = gridHeight g
    x    = i `rem` w
    y    = i `div` w
    here = sampleAt g i

    dx | w <= 1    = 0
       | x == w-1  = here - sampleAt g (i - 1)
       | otherwise = sampleAt g (i + 1) - here

    dy | h <= 1    = 0
       | y == h-1  = here - sampleAt g (i - w)
       | otherwise = sampleAt g (i + w) - here

pixelAt :: Grid -> Int -> Pixel
pixelAt g i =
    let (dx, dy) = slopeAt g i
    in Pixel (sampleAt g i) (Slope (Vec dx dy))
