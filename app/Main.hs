module Main (main) where

import Dem
import Layer

layers = (
    (Water,(LayerBounds 556 557 0.0 0.1),):[])

main :: IO ()
main = do
    -- read arguments and construct config
    -- open source image file
    -- transforme pixels
    -- write to output file
  result <- loadGrid "interlaken.png"
  case result of
    Left err -> putStrLn err
    Right g  -> workWith g

workWith :: Grid -> IO()
workWith img = do
    scaledImage <- scaleWith 556.27 4158.20 img
    let altitudes = concat scaledImage

