module LayerSpec (spec) where

import Test.Hspec

import Codec.Picture (PixelRGB8(..))

import Dem (Pixel(..), Slope(..), Vec(..))
import Layer

layer :: String -> Float -> Float -> Float -> Float -> Layer
layer name loAlt hiAlt loSlope hiSlope =
  Layer name (PixelRGB8 0 0 0) True
    (LayerBounds loAlt hiAlt loSlope hiSlope)

pixel :: Float -> Float -> Pixel
pixel altitude slope = Pixel altitude (Slope (Vec slope 0))

spec :: Spec
spec = describe "determineLayer" $ do
  let lowland = layer "lowland" 0 100 0 1
      mountain = layer "mountain" 100 500 0 1

  it "returns Nothing when there are no layers" $ do
    determineLayer (pixel 50 0) [] `shouldBe` Nothing

  it "selects a layer whose altitude and slope bounds contain the pixel" $ do
    determineLayer (pixel 50 0.5) [lowland] `shouldBe` Just lowland

  it "includes both bounds" $ do
    determineLayer (pixel 0 0) [lowland] `shouldBe` Just lowland
    determineLayer (pixel 100 1) [lowland] `shouldBe` Just lowland

  it "rejects pixels outside the altitude bounds" $ do
    determineLayer (pixel 101 0.5) [lowland] `shouldBe` Nothing
    determineLayer (pixel (-1) 0.5) [lowland] `shouldBe` Nothing

  it "rejects pixels outside the slope bounds" $ do
    determineLayer (pixel 50 1.1) [lowland] `shouldBe` Nothing

  it "uses the norm of the slope vector, not one component" $ do
    let steep = Pixel 50 (Slope (Vec 0.6 0.8)) -- norm 1.0
    determineLayer steep [lowland] `shouldBe` Just lowland

  it "returns the first matching layer when bounds overlap" $ do
    let overlapping = layer "overlapping" 0 500 0 1
    determineLayer (pixel 50 0) [lowland, overlapping] `shouldBe` Just lowland

  it "skips non-matching layers and keeps searching" $ do
    determineLayer (pixel 200 0) [lowland, mountain] `shouldBe` Just mountain
