module DemSpec (spec) where

import Test.Hspec

import Codec.Picture (DynamicImage(..), Pixel8, Pixel16, generateImage)
import qualified Data.Vector.Storable as S
import Data.Word (Word8)

import Dem

-- | Build a Grid8 directly, so tests do not need files on disk.
grid8 :: Int -> Int -> [Word8] -> Grid
grid8 w h values = Grid8 w h (S.fromList values) 0 1

spec :: Spec
spec = do
  describe "grid accessors" $ do
    let g = grid8 3 2 [0, 1, 2, 10, 20, 30]

    it "reports width, height and total cell count" $ do
      gridWidth g `shouldBe` 3
      gridHeight g `shouldBe` 2
      gridCount g `shouldBe` 6

    it "applies the stored offset and factor when reading samples" $ do
      sampleAt (Grid8 1 1 (S.singleton 10) 100 2) 0 `shouldBe` 120

  describe "scaleParams" $ do
    it "maps the raw minimum to altMin and the maximum to altMax" $ do
      let (offset, factor) = scaleParams 556 4158 0 255
      sampleFrom 0 offset factor `shouldBe` 556
      sampleFrom 255 offset factor `shouldBe` 4158

    it "maps a mid-range sample linearly" $ do
      let (offset, factor) = scaleParams 0 100 10 20
      sampleFrom 15 offset factor `shouldBe` 50

    it "maps every sample to altMin for a flat image" $ do
      let (offset, factor) = scaleParams 42 100 7 7
      factor `shouldBe` 0
      sampleFrom 7 offset factor `shouldBe` 42
      sampleFrom 999 offset factor `shouldBe` 42

  describe "scaleWith" $ do
    it "rescales an 8-bit grid using the samples it actually contains" $ do
      let g = scaleWith 0 100 (grid8 2 2 [10, 20, 30, 40])
      sampleAt g 0 `shouldBe` 0
      sampleAt g 3 `shouldBe` 100

    it "rescales a 16-bit grid using the samples it actually contains" $ do
      let g = scaleWith 100 200 (Grid16 1 2 (S.fromList [1000, 3000]) 0 1)
      sampleAt g 0 `shouldBe` 100
      sampleAt g 1 `shouldBe` 200

    it "keeps dimensions" $ do
      let g = scaleWith 0 1 (grid8 4 3 [0 .. 11])
      (gridWidth g, gridHeight g) `shouldBe` (4, 3)

  describe "toGrid" $ do
    it "accepts 8-bit greyscale images" $ do
      let img = generateImage (\_ _ -> 5 :: Pixel8) 2 3
      case toGrid (ImageY8 img) of
        Right g -> do
          (gridWidth g, gridHeight g, gridCount g) `shouldBe` (2, 3, 6)
          sampleAt g 0 `shouldBe` 5
        Left err -> expectationFailure err

    it "accepts 16-bit greyscale images" $ do
      let img = generateImage (\_ _ -> 1000 :: Pixel16) 2 2
      case toGrid (ImageY16 img) of
        Right g -> do
          (gridWidth g, gridHeight g) `shouldBe` (2, 2)
          sampleAt g 3 `shouldBe` 1000
        Left err -> expectationFailure err

    it "rejects non-greyscale images" $ do
      case toGrid (ImageRGB8 (generateImage (\_ _ -> undefined) 1 1)) of
        Left err -> err `shouldBe` "Unsupported format: expected 8 or 16 bit greyscale PNG"
        Right _  -> expectationFailure "expected an error for an RGB image"

  describe "dotProduct / norm" $ do
    it "computes the dot product of two vectors" $ do
      dotProduct (Vec 1 2) (Vec 3 4) `shouldBe` 11

    it "computes the Euclidean norm" $ do
      norm (Vec 3 4) `shouldBe` 5
      norm (Vec 0 0) `shouldBe` 0

  describe "slopeAt" $ do
    it "returns zero slope for a single-cell grid" $ do
      slopeAt (grid8 1 1 [42]) 0 `shouldBe` (0, 0)

    it "uses forward differences inside the grid" $ do
      -- row 0: 10 20 30 / row 1: 40 50 60
      let g = grid8 3 2 [10, 20, 30, 40, 50, 60]
      slopeAt g 0 `shouldBe` (10, 30)
      slopeAt g 4 `shouldBe` (10, 30)

    it "uses backward differences at the right edge" $ do
      let g = grid8 3 2 [10, 20, 30, 40, 50, 60]
      slopeAt g 2 `shouldBe` (10, 30)

    it "uses the matching cell of the previous row on the bottom edge" $ do
      let g = grid8 3 2 [10, 20, 30, 40, 50, 60]
      slopeAt g 5 `shouldBe` (10, 30)

    it "uses zero vertical slope for a single-row grid" $ do
      slopeAt (grid8 3 1 [1, 2, 3]) 1 `shouldBe` (1, 0)

    it "uses zero horizontal slope for a single-column grid" $ do
      slopeAt (grid8 1 3 [1, 2, 3]) 1 `shouldBe` (0, 1)

  describe "pixelAt" $ do
    it "packages altitude and slope into a Pixel" $ do
      let g = grid8 2 2 [10, 20, 30, 50]
      pixelAt g 3 `shouldBe` Pixel 50 (Slope (Vec 20 30))

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

sampleFrom :: Float -> Float -> Float -> Float
sampleFrom sample offset factor = sample * factor + offset
