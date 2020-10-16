module IrishPhonemeInventorySpec where

import Prelude (($), Bool(True))

import Test.Hspec (Spec, describe, it, shouldBe)

import PhonetInventory ( irishPhonetInventoryReport )
import Data.Text (isInfixOf, pack)

irishPhonetInventoryReportSpec :: Spec
irishPhonetInventoryReportSpec = do
	describe "Irish phoneme inventory" $ do
		describe "Consonant phonemes" $ do
			describe "Stop" $ do
				it "should contain /pˠ/" $
					((pack "/pˠ/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
				it "should contain /pʲ/" $
					((pack "/pʲ/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
				it "should contain /t̪ˠ/" $
					((pack "/t̪ˠ/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
				it "should contain /tʲ/" $
					((pack "/tʲ/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
				it "should contain /k/" $
					((pack "/k/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
				it "should contain /c/" $
					((pack "/c/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
				it "should contain /bˠ/" $
					((pack "/bˠ/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
				it "should contain /bʲ/" $
					((pack "/bʲ/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
				it "should contain /d̪ˠ/" $
					((pack "/d̪ˠ/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
				it "should contain /dʲ/" $
					((pack "/dʲ/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
				it "should contain /g/" $
					((pack "/g/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
				it "should contain /ɟ/" $
					((pack "/ɟ/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
			describe "Fricative/Approximant" $ do
				it "should contain /fˠ/" $
					((pack "/fˠ/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
				it "should contain /fʲ/" $
					((pack "/fʲ/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
				it "should contain /sˠ/" $
					((pack "/sˠ/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
				it "should contain /ʃ/" $
					((pack "/ʃ/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
				it "should contain /x/" $
					((pack "/x/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
				it "should contain /ç/" $
					((pack "/ç/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
				it "should contain /h/" $
					((pack "/h/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
				it "should contain /w/" $
					((pack "/w/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
				it "should contain /v/" $
					((pack "/v/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
				it "should contain /vʲ/" $
					((pack "/vʲ/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
				it "should contain /ɣ/" $
					((pack "/ɣ/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
				it "should contain /j/" $
					((pack "/j/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
			describe "Nasal" $ do
				it "should contain /mˠ/" $
					((pack "/mˠ/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
				it "should contain /mʲ/" $
					((pack "/mʲ/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
				it "should contain /n̪ˠ/" $
					((pack "/n̪ˠ/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
				it "should contain /nʲ/" $
					((pack "/nʲ/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
				it "should contain /ŋ/" $
					((pack "/ŋ/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
				it "should contain /ɲ/" $
					((pack "/ɲ/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
			describe "Tap" $ do
				it "should contain /ɾˠ/" $
					((pack "/ɾˠ/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
				it "should contain /ɾʲ/" $
					((pack "/ɾʲ/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
			describe "Lateral" $ do
				it "should contain /l̪ˠ/" $
					((pack "/l̪ˠ/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
				it "should contain /lʲ/" $
					((pack "/lʲ/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
	describe "Vowel phonemes" $ do
		describe "Close" $ do
			it "should contain /ɪ/" $
				((pack "/ɪ/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
			it "should contain /iː/" $
				((pack "/iː/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
			it "should contain /ʊ/" $
				((pack "/ʊ/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
			it "should contain /uː/" $
				((pack "/uː/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
		describe "Mid" $ do
			it "should contain /ɛ/" $
				((pack "/ɛ/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
			it "should contain /eː/" $
				((pack "/eː/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
			it "should contain /ə/" $
				((pack "/ə/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
			it "should contain /ɔ/" $
				((pack "/ɔ/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
			it "should contain /oː/" $
				((pack "/oː/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
		describe "Open" $ do
			it "should contain /a/" $
				((pack "/a/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
			it "should contain /ɑː/" $
				((pack "/ɑː/") `isInfixOf` irishPhonetInventoryReport) `shouldBe` True
