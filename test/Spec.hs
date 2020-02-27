module Spec (main) where

import Prelude (($), (++), IO, length, Maybe(Just), Bool(True, False))
import Test.Hspec (describe, hspec, it, shouldBe, Spec)
    
import Lib(isGlide, analyzeIPA)

main :: IO ()
main =
  hspec glideSpec



glideSpec :: Spec
glideSpec =
  describe "isGlide" $ do
    it "should return true when given [j] the voiced palatal approximant" $
          isGlide (analyzeIPA "j") `shouldBe` True
    it "should return false when given [ʝ] the voiced palatal fricative" $
          isGlide (analyzeIPA "ʝ") `shouldBe` False
    it "should return true when given [w]" $
          isGlide (analyzeIPA "w") `shouldBe` True
