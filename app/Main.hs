module Main (main) where

import Dem
import Shade
import ConfigParser

main :: IO ()
main = do
  result <- loadGrid "./geodata/interlaken.png"
  case result of
    Left err -> putStrLn err
    Right g  -> workWith g

workWith :: Grid -> IO()
workWith grid = do
    configOrErr <- loadConfiguration "./configuration.xml"
    case configOrErr of
        Left err -> putStrLn err
        Right config ->
            let scaled  = scaleWith 556.27 4158.20 grid
                colorAt i = colorAndShade config (pixelAt scaled i)
            in saveImage "aerial.png" (gridWidth scaled) (gridHeight scaled) colorAt
