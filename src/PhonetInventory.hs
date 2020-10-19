module PhonetInventory where

import Prelude(Maybe(..), (<>), map, zip)
import Data.Maybe (maybe)
import Data.Text (Text, concat, pack, unlines)
import DefaultLanguageText ( notApplicableUIText )
import IPA (constructIPA, analyzeIPA)
import GraphemeGrammar(splitIntoPhonemes)
import ShowFunctions (showPhonet)

import LanguageSpecific.EnglishSpecific (englishPhonetInventory)
import LanguageSpecific.ArabicSpecific (arabicPhonemeInventory)
import LanguageSpecific.CreeSpecific (plainsCreePhonemeInventory)
import LanguageSpecific.IrishSpecific (irishPhonemeInventory)

import Types.Phonet ( Phonet(..) )
import Types.PhonetInventory ( PhonetInventory(..) )


{-| 
put a forward slash before some text  and after it. For example, "s" becomes "/s/". Linguists use these forward slashes to indicate a phonemic transcription, instead of a phonetic transcription.
-}
encloseInSlashes :: Text -> Text
encloseInSlashes ipaText = pack "/" <> ipaText <> pack "/"

ipaAndPhonetFormat :: (Text, Maybe Phonet) -> Text
ipaAndPhonetFormat (ipaText, phonet) =
	encloseInSlashes ipaText <> pack " " <> phonetSummary
	where
	phonetSummary =
		maybe notApplicableUIText showPhonet phonet

ipaTextToPhonetList :: Text -> [(Text, Maybe Phonet)]
ipaTextToPhonetList text =
	let 
	{
		ipaChunks = splitIntoPhonemes text;
		phonetes = map analyzeIPA ipaChunks;
	}
	in zip ipaChunks phonetes


{-| 
Given text containing international phonetic alphabet symbols returns text with every phonetic alphabet symbol or sequence of symbols for a sound followed by the description of the sound it represents.
-}
ipaTextToPhonetListReport :: Text -> Text
ipaTextToPhonetListReport text =
	let listA = ipaTextToPhonetList text
	in unlines (map ipaAndPhonetFormat listA)


showIPA :: PhonetInventory -> Text
showIPA p = concat (showIPAAsList p)

showIPAAsList :: PhonetInventory -> [Text]
showIPAAsList (PhonetInventory phonetes) = map constructIPA phonetes


englishPhonetInventoryReport :: Text
englishPhonetInventoryReport = ipaTextToPhonetListReport (showIPA englishPhonetInventory)

arabicPhonetInventoryReport :: Text
arabicPhonetInventoryReport = ipaTextToPhonetListReport (showIPA arabicPhonemeInventory)

plainsCreePhonetInventoryReport :: Text
plainsCreePhonetInventoryReport = ipaTextToPhonetListReport (showIPA plainsCreePhonemeInventory)

irishPhonetInventoryReport :: Text
irishPhonetInventoryReport = ipaTextToPhonetListReport (showIPA irishPhonemeInventory)

