{-# LANGUAGE OverloadedStrings #-}

module ConfigParserSpec (spec) where

import Test.Hspec

import Codec.Picture (PixelRGB8(..))
import qualified Data.ByteString as BS

import ConfigParser (parseConfiguration)
import Dem (Vec(..))
import Layer

validConfig :: BS.ByteString
validConfig = "<HaskapeConfiguration>\
\  <SunDirection><dx>0.25</dx><dy>-0.5</dy></SunDirection>\
\  <shadeStrength>0.4</shadeStrength>\
\  <fallBackColor>#F0F0F0</fallBackColor>\
\  <Layers>\
\    <Layer>\
\      <name>Water</name><color>#0000FF</color><shade>False</shade>\
\      <Bounds><minAltitude>556</minAltitude><maxAltitude>565</maxAltitude>\
\               <minSlope>0.0</minSlope><maxSlope>1.0</maxSlope></Bounds>\
\    </Layer>\
\    <Layer>\
\      <name>Grass</name><color>#46A028</color><shade>True</shade>\
\      <Bounds><minAltitude>565</minAltitude><maxAltitude>2800</maxAltitude>\
\               <minSlope>0.0</minSlope><maxSlope>0.8</maxSlope></Bounds>\
\    </Layer>\
\  </Layers>\
\</HaskapeConfiguration>"

spec :: Spec
spec = describe "parseConfiguration" $ do
  it "parses a complete configuration" $ do
    case parseConfiguration validConfig of
      Left err -> expectationFailure err
      Right config -> do
        config `shouldBe` HaskapeConfiguration
          (Vec 0.25 (-0.5)) 0.4 (PixelRGB8 240 240 240)
          [ Layer "Water" (PixelRGB8 0 0 255) False
              (LayerBounds 556 565 0 1)
          , Layer "Grass" (PixelRGB8 70 160 40) True
              (LayerBounds 565 2800 0 0.8)
          ]

  it "accepts a configuration with no layers" $ do
    let xml = "<HaskapeConfiguration>\
\                <SunDirection><dx>1</dx><dy>2</dy></SunDirection>\
\                <shadeStrength>0</shadeStrength>\
\                <fallBackColor>#FFFFFF</fallBackColor>\
\                <Layers></Layers>\
\              </HaskapeConfiguration>"
    case parseConfiguration xml of
      Left err -> expectationFailure err
      Right (HaskapeConfiguration _ _ _ layers) -> layers `shouldBe` []

  it "accepts case-insensitive booleans and lowercase hex colors" $ do
    let xml = replace "#F0F0F0" "#abcdef" (replace "True" "tRuE" validConfig)
    case parseConfiguration xml of
      Left err -> expectationFailure err
      Right config -> case configurationLayers config of
        [water, grass] -> do
          layerShade water `shouldBe` False
          layerShade grass `shouldBe` True
          configFallback config `shouldBe` PixelRGB8 171 205 239
        layers -> expectationFailure ("unexpected layers: " ++ show layers)

  it "rejects an unexpected root element" $ do
    let xml = "<WrongRoot></WrongRoot>"
    parseConfiguration xml `shouldBe`
      Left "Expected root <HaskapeConfiguration>, found <WrongRoot>"

  it "reports a missing child element" $ do
    let xml = "<HaskapeConfiguration></HaskapeConfiguration>"
    parseConfiguration xml `shouldBe`
      Left "Missing <SunDirection> inside <HaskapeConfiguration>"

  it "reports a non-numeric field" $ do
    let xml = "<HaskapeConfiguration>\
\                <SunDirection><dx>abc</dx><dy>0</dy></SunDirection>\
\              </HaskapeConfiguration>"
    parseConfiguration xml `shouldBe`
      Left "Cannot parse <dx> value: \"abc\""

  it "reports an invalid boolean" $ do
    let xml = replace "False" "maybe" validConfig
    parseConfiguration xml `shouldBe` Left "Invalid boolean: \"maybe\""

  it "reports an invalid color" $ do
    let xml = replace "#F0F0F0" "#GGGGGG" validConfig
    parseConfiguration xml `shouldBe`
      Left "Invalid color (expected #RRGGBB): \"#GGGGGG\""

  it "rejects a color with the wrong number of digits" $ do
    let xml = replace "#F0F0F0" "#FFF" validConfig
    parseConfiguration xml `shouldBe`
      Left "Invalid color (expected #RRGGBB): \"#FFF\""

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

configurationLayers :: HaskapeConfiguration -> [Layer]
configurationLayers (HaskapeConfiguration _ _ _ layers) = layers

configFallback :: HaskapeConfiguration -> PixelRGB8
configFallback (HaskapeConfiguration _ _ color _) = color

layerShade :: Layer -> Bool
layerShade (Layer _ _ shade _) = shade

replace :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
replace needle replacement haystack = BS.intercalate replacement (split haystack)
  where
    split bs
      | BS.null bs = [BS.empty]
      | otherwise =
          case BS.breakSubstring needle bs of
            (prefix, rest)
              | BS.null rest -> [prefix]
              | otherwise -> prefix : split (BS.drop (BS.length needle) rest)
