module ConfigParser (loadConfiguration, parseConfiguration) where

import Codec.Picture (PixelRGB8(..))
import qualified Data.ByteString as BS
import Data.Char (digitToInt, isHexDigit, isSpace, toLower)
import Text.Read (readMaybe)
import Text.XML.Light
import Layer(Layer(..),LayerBounds(..),HaskapeConfiguration(..))
import Dem(Vec(..))

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

loadConfiguration :: FilePath -> IO (Either String HaskapeConfiguration)
loadConfiguration path = parseConfiguration <$> BS.readFile path

parseConfiguration :: BS.ByteString -> Either String HaskapeConfiguration
parseConfiguration bytes = do
  root <- maybe (Left "Invalid XML document") Right (parseXMLDoc bytes)
  expectName "HaskapeConfiguration" root

  sunEl  <- child "SunDirection" root
  dx     <- field "dx" sunEl
  dy     <- field "dy" sunEl

  shadeS <- field "shadeStrength" root
  fallBk <- text "fallBackColor" root >>= parseColor
  layersEl <- child "Layers" root
  layers <- mapM parseLayer (findChildren (unqual "Layer") layersEl)

  Right (HaskapeConfiguration (Vec dx dy) shadeS fallBk layers)
  -- ^ adjust `Vec dx dy` to however your Vec type is constructed

-- ---------------------------------------------------------------------------
-- Layers
-- ---------------------------------------------------------------------------

parseLayer :: Element -> Either String Layer
parseLayer el = do
  name   <- text "name" el
  color  <- text "color" el >>= parseColor
  shade  <- text "shade" el >>= parseBool
  bounds <- child "Bounds" el >>= parseBounds
  Right (Layer name color shade bounds)

parseBounds :: Element -> Either String LayerBounds
parseBounds el =
  LayerBounds
    <$> field "minAltitude" el
    <*> field "maxAltitude" el
    <*> field "minSlope" el
    <*> field "maxSlope" el

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

expectName :: String -> Element -> Either String ()
expectName expected el
  | qName (elName el) == expected = Right ()
  | otherwise = Left $ "Expected root <" ++ expected ++ ">, found <"
                       ++ qName (elName el) ++ ">"

child :: String -> Element -> Either String Element
child name el =
  maybe (Left $ "Missing <" ++ name ++ "> inside <" ++ qName (elName el) ++ ">")
        Right
        (findChild (unqual name) el)

-- | Trimmed text content of a child element.
text :: String -> Element -> Either String String
text name el = trim . strContent <$> child name el

-- | Read any 'Read'able value (Float, Int, ...) from a child element.
field :: Read a => String -> Element -> Either String a
field name el = do
  s <- text name el
  maybe (Left $ "Cannot parse <" ++ name ++ "> value: " ++ show s) Right (readMaybe s)

parseBool :: String -> Either String Bool
parseBool s = case map toLower s of
  "true"  -> Right True
  "false" -> Right False
  _       -> Left $ "Invalid boolean: " ++ show s

-- | "#RRGGBB" -> PixelRGB8
parseColor :: String -> Either String PixelRGB8
parseColor ('#' : cs@[r1, r2, g1, g2, b1, b2])
  | all isHexDigit cs = Right (PixelRGB8 (byte r1 r2) (byte g1 g2) (byte b1 b2))
  where byte hi lo = fromIntegral (digitToInt hi * 16 + digitToInt lo)
parseColor s = Left $ "Invalid color (expected #RRGGBB): " ++ show s

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace