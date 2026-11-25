module CommandLine
  ( Options(..)
  , emptyOptions
  , Config(..)
  , optionDescriptions
  , usageHeader
  , validate
  , parseAltitudeRange
  , trim
  ) where

import Data.Char (isSpace)
import System.Console.GetOpt
  ( ArgDescr(..), OptDescr(..) )
import Text.Read (readMaybe)

-- | Raw command-line state: options are filled in as they are encountered.
data Options = Options
  { optConfig :: Maybe FilePath
  , optInput  :: Maybe FilePath
  , optOutput :: Maybe FilePath
  , optRange  :: Maybe String
  , optHelp   :: Bool
  , optOutputWidth :: Maybe Int
  }

emptyOptions :: Options
emptyOptions = Options Nothing Nothing Nothing Nothing False Nothing

-- | Validated command-line arguments, ready to drive the conversion.
data Config = Config
  { configPath  :: FilePath
  , inputPath   :: FilePath
  , outputPath  :: FilePath
  , altitudeMin :: Float
  , altitudeMax :: Float
  , outputWidth :: Maybe Int
  } deriving (Show, Eq)

optionDescriptions :: [OptDescr (Options -> Options)]
optionDescriptions =
  [ Option ['c'] ["config"]
      (ReqArg (\value opts -> opts { optConfig = Just value }) "FILE")
      "configuration XML file (see README.md)"
  , Option ['i'] ["input"]
      (ReqArg (\value opts -> opts { optInput = Just value }) "FILE")
      "input greyscale elevation image"
  , Option ['o'] ["output"]
      (ReqArg (\value opts -> opts { optOutput = Just value }) "FILE")
      "output PNG image"
  , Option ['a'] ["altitude-range"]
      (ReqArg (\value opts -> opts { optRange = Just value }) "MIN,MAX")
      "altitudes mapped to the darkest and lightest input samples"
  , Option ['w'] ["output-width"]
      (OptArg (\value opts -> case value of
        Nothing -> opts { optOutputWidth = Nothing}
        Just str -> opts { optOutputWidth = Just $ read str}) "WIDTH")
      "width of the output image, use to generate smaller images"
  , Option ['h'] ["help"]
      (NoArg (\opts -> opts { optHelp = True }))
      "show this help and exit"
  ]

usageHeader :: String -> String
usageHeader prog =
  "Usage: " ++ prog ++
  " --config FILE --input FILE --output FILE --altitude-range MIN,MAX"

validate :: [String] -> [String] -> Options -> Either String Config
validate leftovers optErrors opts =
  case optErrors ++ map ("unexpected argument: " ++) leftovers of
    err : _ -> Left err
    [] -> do
      cfgFile <- require "--config" (optConfig opts)
      inFile  <- require "--input"  (optInput opts)
      outFile <- require "--output" (optOutput opts)
      rawRange <- require "--altitude-range" (optRange opts)
      (lo, hi) <- parseAltitudeRange rawRange
      pure Config
        { configPath  = cfgFile
        , inputPath   = inFile
        , outputPath  = outFile
        , altitudeMin = lo
        , altitudeMax = hi
        , outputWidth = optOutputWidth opts
        }
    where
        require :: String -> Maybe a -> Either String a
        require name = maybe (Left $ "missing required option " ++ name) Right

parseAltitudeRange :: String -> Either String (Float, Float)
parseAltitudeRange raw =
  case break (== ',') raw of
    (lo, ',' : hi)
      | ',' `notElem` hi -> do
          lo' <- altitude "minimum" lo
          hi' <- altitude "maximum" hi
          if lo' <= hi'
            then Right (lo', hi')
            else Left "minimum altitude must not exceed maximum altitude"
    _ -> Left "expected two comma-separated numbers, e.g. 556.2,4187.1"
  where
    altitude :: String -> String -> Either String Float
    altitude label value =
      case readMaybe (trim value) of
        Just number | not (isNaN number) && not (isInfinite number) -> Right number
        _ -> Left $ "invalid " ++ label ++ " altitude: " ++ show value

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
