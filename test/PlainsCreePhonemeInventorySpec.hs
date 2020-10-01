module PlainsCreePhonemeInventorySpec where

import Prelude (IO, ($), (++), (.), (||), String, Bool(True, False), Maybe(Just, Nothing), map)
import           Data.Maybe    (fromJust)
import           Test.Hspec    (Spec, describe, hspec, it, shouldBe)
import           IPA  ( plainsCreePhonetInventoryReport )
import Data.Text (Text, pack, unpack, isInfixOf)

plainsCreePhonetInventoryReportSpec :: Spec
plainsCreePhonetInventoryReportSpec = do
  describe "Plains Cree phoneme inventory" $ do
    it "should contain /p/" $
      ((pack "/p/") `isInfixOf` plainsCreePhonetInventoryReport) `shouldBe` True
    it "should contain /t/" $
      ((pack "/t/") `isInfixOf` plainsCreePhonetInventoryReport) `shouldBe` True
    it "should contain /k/" $
      ((pack "/k/") `isInfixOf` plainsCreePhonetInventoryReport) `shouldBe` True
    it "should contain /ʔ/" $
      ((pack "/ʔ/") `isInfixOf` plainsCreePhonetInventoryReport) `shouldBe` True
    it "should contain /t͜s/" $
       (pack "/t͜s/") `isInfixOf` plainsCreePhonetInventoryReport  `shouldBe` True
    it "should contain /m/" $
      ((pack "/m/") `isInfixOf` plainsCreePhonetInventoryReport) `shouldBe` True
    it "should contain /n/" $
      ((pack "/n/") `isInfixOf` plainsCreePhonetInventoryReport) `shouldBe` True
    it "should contain /h/" $
      ((pack "/h/") `isInfixOf` plainsCreePhonetInventoryReport) `shouldBe` True
    it "should contain /w/" $
      ((pack "/w/") `isInfixOf` plainsCreePhonetInventoryReport) `shouldBe` True
    it "should contain /j/" $
      ((pack "/j/") `isInfixOf` plainsCreePhonetInventoryReport) `shouldBe` True
    it "should contain /i/" $
      ((pack "/i/") `isInfixOf` plainsCreePhonetInventoryReport) `shouldBe` True
    it "should contain /u/" $
      ((pack "/u/") `isInfixOf` plainsCreePhonetInventoryReport) `shouldBe` True
    it "should contain /a/" $
      ((pack "/a/") `isInfixOf` plainsCreePhonetInventoryReport) `shouldBe` True
    it "should contain /iː/" $
      ((pack "/iː/") `isInfixOf` plainsCreePhonetInventoryReport) `shouldBe` True
    it "should contain /eː/" $
      ((pack "/eː/") `isInfixOf` plainsCreePhonetInventoryReport) `shouldBe` True
    it "should contain /aː/" $
      ((pack "/aː/") `isInfixOf` plainsCreePhonetInventoryReport) `shouldBe` True
    it "should contain /oː/" $
      ((pack "/oː/") `isInfixOf` plainsCreePhonetInventoryReport) `shouldBe` True
