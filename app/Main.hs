module Main (main) where

import Codec.Picture (PixelRGB8(..))
import Dem
import Layer
import Shade

layers :: Config
layers = [(Water, LayerBounds 556 557 0.0 0.1, PixelRGB8 0 0 255)]

main :: IO ()
main = do
    -- read arguments and construct config
    -- open source image file
    -- transforme pixels
    -- write to output file
  result <- loadGrid "./geodata/interlaken.png"
  case result of
    Left err -> putStrLn err
    Right g  -> workWith g

workWith :: Grid -> IO()
workWith img = do
    let scaledImage = scaleWith 556.27 4158.20 img
        allPixels   = pixels scaledImage --flat representation of the image with altitude and slope
        finalImageColors = map (colorAndShade layers) allPixels
    saveImage "aerial.png" (length img) (length $ head img) finalImageColors
