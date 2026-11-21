module Dem where

import Codec.Picture
import Data.List (transpose)

type Grid = [[FLoat]]

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

imageToGrid :: (Pixel a, Integral a) => Image a -> Grid
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
    scale p = (p - imgMin) * factor + altMin

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
