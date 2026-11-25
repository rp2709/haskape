module ShadeSpec (spec) where

import Test.Hspec

import Codec.Picture (PixelRGB8(..))
import qualified Data.Vector as V

import Dem (Pixel(..), Slope(..), Vec(..))
import Layer
import Shade

white, black, midGray :: PixelRGB8
white = PixelRGB8 255 255 255
black = PixelRGB8 0 0 0
midGray = PixelRGB8 128 128 128

config :: [Layer] -> HaskapeConfiguration
config = HaskapeConfiguration (Vec 1 0) 1 (PixelRGB8 200 100 50)

unshadedLayer :: Layer
unshadedLayer =
  Layer "flat" white False (LayerBounds 0 100 0 1)

shadedLayer :: Layer
shadedLayer =
  Layer "shaded" (PixelRGB8 100 100 100) True (LayerBounds 0 100 0 1)

spec :: Spec
spec = do
  describe "cosPhi" $ do
    it "returns 1 for parallel vectors" $ do
      cosPhi (Vec 1 0) (Vec 2 0) `shouldBe` 1

    it "returns -1 for opposite vectors" $ do
      cosPhi (Vec 1 0) (Vec (-1) 0) `shouldBe` (-1)

    it "returns 0 for perpendicular vectors" $ do
      cosPhi (Vec 1 0) (Vec 0 1) `shouldBe` 0

    it "returns 0 when one vector has zero length" $ do
      cosPhi (Vec 0 0) (Vec 1 1) `shouldBe` 0

  describe "luminescence" $ do
    it "leaves the color unchanged at value 0" $ do
      luminescence 1 midGray 0 `shouldBe` midGray

    it "blends toward white at full positive value" $ do
      luminescence 1 black 1 `shouldBe` white
      luminescence 1 midGray 1 `shouldBe` white

    it "blends toward black at full negative value" $ do
      luminescence 1 white (-1) `shouldBe` black

    it "scales the effect with strength" $ do
      luminescence 0.5 black 1 `shouldBe` PixelRGB8 128 128 128
      luminescence 0.5 white (-1) `shouldBe` PixelRGB8 128 128 128

    it "clamps values outside [-1, 1]" $ do
      luminescence 1 black 20 `shouldBe` white
      luminescence 1 white (-20) `shouldBe` black

    it "scales positive blending by strength independently of the input channel" $ do
      luminescence 0.5 (PixelRGB8 10 10 10) 1 `shouldBe` PixelRGB8 132 132 132 -- 10 + (255-10)*0.5 = 132.5, rounded to even
      luminescence 0.5 (PixelRGB8 200 200 200) 1 `shouldBe` PixelRGB8 228 228 228

    it "never produces out-of-range channels" $ do
      let PixelRGB8 r g b = luminescence 2 (PixelRGB8 10 20 30) 0.9
      all (`elem` [0 .. 255]) [r, g, b] `shouldBe` True

  describe "shade" $ do
    it "brightens a surface facing the sun" $ do
      shade 1 midGray (Slope (Vec 1 0)) (Vec 1 0) `shouldBe` white

    it "darkens a surface facing away from the sun" $ do
      shade 1 midGray (Slope (Vec (-1) 0)) (Vec 1 0) `shouldBe` black

    it "does not change a color when strength is zero" $ do
      shade 0 midGray (Slope (Vec 1 0)) (Vec 1 0) `shouldBe` midGray

  describe "colorAndShade" $ do
    let flatPixel = Pixel 50 (Slope (Vec 0 0))

    it "uses the shaded fallback color when no layer matches" $ do
      let p = Pixel 200 (Slope (Vec 1 0)) -- slope aligned with the sun
          PixelRGB8 r _ _ = colorAndShade (config [unshadedLayer]) p
      r `shouldBe` 255 -- fallback (200,100,50) blended toward white

    it "uses a layer color directly when shade is False" $ do
      colorAndShade (config [unshadedLayer]) flatPixel `shouldBe` white

    it "shades the layer color when shade is True" $ do
      let p = Pixel 50 (Slope (Vec 1 0))
          PixelRGB8 r _ _ = colorAndShade (config [shadedLayer]) p
      r `shouldBe` 255 -- 100 lerped toward white with value 1

    it "uses the first matching layer" $ do
      let second = Layer "second" black True (LayerBounds 0 100 0 1)
      colorAndShade (config [unshadedLayer, second]) flatPixel `shouldBe` white

  describe "scaleDown" $ do
    it "leaves the image unchanged when the requested width is not smaller" $ do
      let src = V.fromList [white, black, midGray, white]
          (out, size) = scaleDown src (2, 2) 2
      (out, size) `shouldBe` (src, (2, 2))

    it "leaves the image unchanged for a non-positive width" $ do
      let src = V.singleton white
      scaleDown src (1, 1) 0 `shouldBe` (src, (1, 1))

    it "reduces width and preserves the aspect ratio" $ do
      let src = V.generate (4 * 6) (const white)
          (out, size) = scaleDown src (4, 6) 2
      (V.length out, size) `shouldBe` (6, (2, 3))

    it "keeps at least one row for very wide images" $ do
      let src = V.generate 100 (const white)
      snd (scaleDown src (100, 1) 2) `shouldBe` (2, 1)

    it "averages colors over the source block" $ do
      -- 2x2 image down to 1x1: (10 + 20 + 30 + 40) / 4 = 25
      let src = V.fromList
            [ PixelRGB8 10 10 10, PixelRGB8 20 20 20
            , PixelRGB8 30 30 30, PixelRGB8 40 40 40 ]
          (out, size) = scaleDown src (2, 2) 1
      size `shouldBe` (1, 1)
      out V.! 0 `shouldBe` PixelRGB8 25 25 25
