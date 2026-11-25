module Main (main) where

import Control.Exception (IOException, try)
import System.Console.GetOpt (ArgOrder(..), getOpt, usageInfo)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import qualified Data.Vector as V

import CommandLine
import ConfigParser (loadConfiguration)
import Dem
import Layer (HaskapeConfiguration)
import Shade

parseCommandLine :: IO Config
parseCommandLine = do
  args <- getArgs
  prog <- getProgName
  let descriptors = optionDescriptions
      (updates, leftovers, optErrors) = getOpt Permute descriptors args
      parsed = foldl (flip ($)) emptyOptions updates
  if optHelp parsed
    then putStr (usageInfo (usageHeader prog) descriptors) >> exitSuccess
    else case validate leftovers optErrors parsed of
      Left err -> do
        hPutStrLn stderr ("Error: " ++ err)
        hPutStrLn stderr (usageInfo (usageHeader prog) descriptors)
        exitFailure
      Right cfg -> pure cfg

main :: IO ()
main = do
  cfg <- parseCommandLine

  gridResult <- try (loadGrid (inputPath cfg))
                   :: IO (Either IOException (Either String Grid))
  grid <- case gridResult of
    Left ex         -> dieWith ("cannot read input image: " ++ show ex)
    Right (Left err)-> dieWith err
    Right (Right g) -> pure g

  configResult <- try (loadConfiguration (configPath cfg))
                    :: IO (Either IOException (Either String HaskapeConfiguration))
  config <- case configResult of
    Left ex          -> dieWith ("cannot read configuration: " ++ show ex)
    Right (Left err) -> dieWith err
    Right (Right c)  -> pure c

  let scaled  = scaleWith (altitudeMin cfg) (altitudeMax cfg) grid
  case outputWidth cfg of
    Nothing ->
      let colorAt i = colorAndShade config (pixelAt scaled i)
      in saveImage (outputPath cfg) (gridWidth scaled) (gridHeight scaled) colorAt
    Just ow ->
      let fullSize = V.generate (gridCount scaled) (\i -> colorAndShade config (pixelAt scaled i))
          (lr,(lrWidth,lrHeight)) = scaleDown fullSize ((gridWidth scaled),(gridHeight scaled)) ow
      in saveImage (outputPath cfg) lrWidth lrHeight (lr V.!)


dieWith :: String -> IO a
dieWith message = hPutStrLn stderr message >> exitFailure
