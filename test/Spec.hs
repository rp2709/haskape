module Main (main) where

import Test.Hspec

import qualified ConfigParserSpec
import qualified DemSpec
import qualified LayerSpec
import qualified MainSpec
import qualified ShadeSpec

main :: IO ()
main = hspec $ do
  describe "ConfigParser" ConfigParserSpec.spec
  describe "Dem" DemSpec.spec
  describe "Layer" LayerSpec.spec
  describe "Shade" ShadeSpec.spec
  describe "Main (CLI)" MainSpec.spec
