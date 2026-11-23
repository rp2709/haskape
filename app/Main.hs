module Main (main) where

import Codec.Picture (PixelRGB8(..))
import Dem
import Layer
import Shade

layers :: Config
layers = [
    (Water, LayerBounds 556 565 0.0 1, PixelRGB8 0 0 255),
    (Grass, LayerBounds 565 2800 0.0 0.8, PixelRGB8 70 160 40),
    (Forest, LayerBounds 565 2800 0.8 1.2, PixelRGB8 190 90 30),
    (Snow, LayerBounds 2800 5000 0.0 1.0, PixelRGB8 240 245 245),
    (Rocks, LayerBounds 565 5000 1.2 1000.0, PixelRGB8 70 100 100)]

main :: IO ()
main = do
  result <- loadGrid "./geodata/interlaken.png"
  case result of
    Left err -> putStrLn err
    Right g  -> workWith g

workWith :: Grid -> IO()
workWith grid = do
    let scaled  = scaleWith 556.27 4158.20 grid
        colorAt i = colorAndShade layers (pixelAt scaled i)
    saveImage "aerial.png" (gridWidth scaled) (gridHeight scaled) colorAt
