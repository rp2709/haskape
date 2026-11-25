module MainSpec (spec) where

import Test.Hspec

import CommandLine (parseAltitudeRange, trim, validate, Config(..), Options(..), emptyOptions)

spec :: Spec
spec = do
  describe "trim" $ do
    it "removes leading and trailing whitespace" $ do
      trim "  42 \t" `shouldBe` "42"

    it "keeps internal whitespace" $ do
      trim "a b" `shouldBe` "a b"

  describe "parseAltitudeRange" $ do
    it "parses two comma-separated numbers" $ do
      parseAltitudeRange "556.2,4187.1" `shouldBe` Right (556.2, 4187.1)

    it "accepts whitespace around the numbers" $ do
      parseAltitudeRange " 0 , 100 " `shouldBe` Right (0, 100)

    it "accepts equal bounds" $ do
      parseAltitudeRange "5,5" `shouldBe` Right (5, 5)

    it "rejects a missing comma" $ do
      parseAltitudeRange "100" `shouldBe`
        Left "expected two comma-separated numbers, e.g. 556.2,4187.1"

    it "rejects more than one comma" $ do
      parseAltitudeRange "1,2,3" `shouldBe`
        Left "expected two comma-separated numbers, e.g. 556.2,4187.1"

    it "rejects a missing maximum" $ do
      parseAltitudeRange "1," `shouldBe`
        Left "invalid maximum altitude: \"\""

    it "rejects non-numeric values" $ do
      parseAltitudeRange "abc,2" `shouldBe`
        Left "invalid minimum altitude: \"abc\""

    it "rejects NaN and infinity" $ do
      parseAltitudeRange "NaN,2" `shouldBe`
        Left "invalid minimum altitude: \"NaN\""
      parseAltitudeRange "1,Infinity" `shouldBe`
        Left "invalid maximum altitude: \"Infinity\""

    it "rejects a descending range" $ do
      parseAltitudeRange "10,5" `shouldBe`
        Left "minimum altitude must not exceed maximum altitude"

  describe "validate" $ do
    let complete = emptyOptions
          { optConfig = Just "config.xml"
          , optInput = Just "input.png"
          , optOutput = Just "output.png"
          , optRange = Just "0,100"
          , optOutputWidth = Just 50
          }

    it "builds a Config from complete options" $ do
      validate [] [] complete `shouldBe` Right Config
        { configPath = "config.xml"
        , inputPath = "input.png"
        , outputPath = "output.png"
        , altitudeMin = 0
        , altitudeMax = 100
        , outputWidth = Just 50
        }

    it "uses Nothing for output width when not supplied" $ do
      let result = validate [] [] complete { optOutputWidth = Nothing }
      fmap outputWidth result `shouldBe` Right Nothing

    it "reports a missing required option" $ do
      validate [] [] complete { optInput = Nothing } `shouldBe`
        Left "missing required option --input"

    it "reports unexpected positional arguments" $ do
      validate ["extra"] [] complete `shouldBe`
        Left "unexpected argument: extra"

    it "reports option parsing errors" $ do
      validate [] ["unrecognized option"] complete `shouldBe`
        Left "unrecognized option"
