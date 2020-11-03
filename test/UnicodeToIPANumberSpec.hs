module UnicodeToIPANumberSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import UnicodeToIPANumber (unicodeToNumber)

unicodeToIPANumberSpec :: Spec
unicodeToIPANumberSpec =
	describe "Convert Unicode code points to IPA Numbers"
		(
			do
			{
				it "latin small letter p to IPA Number 101"
					(unicodeToNumber 'p' `shouldBe` 101);
				it "latin small letter p code point to IPA Number 101"
					(unicodeToNumber '\x0070' `shouldBe` 101);
				it "latin small letter b to IPA Number 102"
					(unicodeToNumber 'b' `shouldBe` 102);
				it "latin small letter b code point to IPA Number 102"
					(unicodeToNumber '\x0062' `shouldBe` 102);
				it "latin small letter t to IPA Number 103"
					(unicodeToNumber 't' `shouldBe` 103);
				it "latin small letter t code point to IPA Number 103"
					(unicodeToNumber '\x0074' `shouldBe` 103);
				it "latin small letter d to IPA Number 104"
					(unicodeToNumber 'd'  `shouldBe` 104);
				it "latin small letter d code point to IPA Number 104"
					(unicodeToNumber '\x0064' `shouldBe` 104);
				it "latin small letter t with retroflex hook to IPA Number 105"
					(unicodeToNumber 'ʈ' `shouldBe` 105);
				it "latin small letter t with retroflex hook code point to IPA Number 105"
					(unicodeToNumber '\x0288' `shouldBe` 105);
				it "latin small letter d with tail to IPA Number 106"
					(unicodeToNumber 'ɖ' `shouldBe` 106);
				it "latin small letter d with tail code point to IPA Number 106"
					(unicodeToNumber '\x0256' `shouldBe` 106);
				it "latin small letter c to IPA Number 107"
					(unicodeToNumber 'c' `shouldBe` 107);
				it "latin small letter c code point to IPA Number 107"
					(unicodeToNumber '\x0063' `shouldBe` 107);
				it "latin small dotless j with stroke to IPA Number 108"
					(unicodeToNumber 'ɟ' `shouldBe` 108);
				it "latin small dotless j with stroke code point to IPA Number 108"
					(unicodeToNumber '\x025F' `shouldBe` 108);
				it "latin small letter k to IPA Number 109"
					(unicodeToNumber 'k' `shouldBe` 109);
				it "latin small letter k code point to IPA Number 109"
					(unicodeToNumber '\x006B' `shouldBe` 109);
				it "latin small letter g to IPA Number 110"
					(unicodeToNumber 'g' `shouldBe` 110);
				it "latin small letter g code point to IPA Number 110"
					(unicodeToNumber '\x0067' `shouldBe` 110);
				it "latin small letter script g to IPA Number 110"
					(unicodeToNumber 'ɡ' `shouldBe` 110);
				it "latin small letter script g code point to IPA Number 110"
					(unicodeToNumber '\x0261' `shouldBe` 110);
				it "latin small letter q to IPA Number 111"
					(unicodeToNumber 'q' `shouldBe` 111);
				it "latin small letter q code point to IPA Number 111"
					(unicodeToNumber '\x0071' `shouldBe` 111);
				it "latin letter small capital g to IPA Number 112"
					(unicodeToNumber 'ɢ' `shouldBe` 112);
				it "latin letter small capital g code point to IPA Number 112"
					(unicodeToNumber '\x0262' `shouldBe` 112);
				it "latin letter glottal stop to IPA Number 113"
					(unicodeToNumber 'ʔ' `shouldBe` 113);
				it "latin letter glottal stop code point to IPA Number 113"
					(unicodeToNumber '\x0294' `shouldBe` 113);
				it "latin small letter m to IPA Number 114"
					(unicodeToNumber 'm' `shouldBe` 114);
				it "latin small letter m code point to IPA Number 114"
					(unicodeToNumber '\x006D' `shouldBe` 114);
				it "latin small letter m with hook to IPA Number 115"
					(unicodeToNumber 'ɱ' `shouldBe` 115);
				it "latin small letter m with hook code point to IPA Number 115"
					(unicodeToNumber '\x0271' `shouldBe` 115);
				it "latin small letter n to IPA Number 116"
					(unicodeToNumber 'n' `shouldBe` 116);
				it "latin small letter n code point to IPA Number 116"
					(unicodeToNumber '\x006E' `shouldBe` 116);
				it "latin small letter n with retroflex hook to IPA Number 117"
					(unicodeToNumber 'ɳ' `shouldBe` 117);
				it "latin small letter n with retroflex hook code point to IPA Number 117"
					(unicodeToNumber '\x0273' `shouldBe` 117);
				it "latin small letter n with left hook to IPA Number 118"
					(unicodeToNumber 'ɲ' `shouldBe` 118);
				it "latin small letter n with left hook code point to IPA Number 118"
					(unicodeToNumber '\x0272' `shouldBe` 118);
				it "latin small letter eng to IPA Number 119"
					(unicodeToNumber 'ŋ' `shouldBe` 119);
				it "latin small letter eng code point to IPA Number 119"
					(unicodeToNumber '\x014B' `shouldBe` 119);
				it "latin letter small capital n to IPA Number 120"
					(unicodeToNumber 'ɴ' `shouldBe` 120);
				it "latin letter small capital n code point to IPA Number 120"
					(unicodeToNumber '\x0274' `shouldBe` 120);
				it "latin letter small capital b to IPA Number 121"
					(unicodeToNumber 'ʙ' `shouldBe` 121);
				it "latin letter small capital b code point to IPA Number 121"
					(unicodeToNumber '\x0299' `shouldBe` 121);
				it "latin small letter r to IPA Number 122"
					(unicodeToNumber 'r' `shouldBe` 122);
				it "latin small letter r code point to IPA Number 122"
					(unicodeToNumber '\x0072' `shouldBe` 122);
				it "latin letter small capital r to IPA Number 123"
					(unicodeToNumber 'ʀ' `shouldBe` 123);
				it "latin letter small capital r code point to IPA Number 123"
					(unicodeToNumber '\x0280' `shouldBe` 123);
				it "latin small letter v with right hook to IPA Number 184"
					(unicodeToNumber 'ⱱ' `shouldBe` 184);
				it "latin small letter v with right hook code point to IPA Number 184"
					(unicodeToNumber '\x2C71' `shouldBe` 184);
				it "latin small letter r with fishhook to IPA Number 124"
					(unicodeToNumber 'ɾ' `shouldBe` 124);
				it "latin small letter r with fishhook code point to IPA Number 124"
					(unicodeToNumber '\x027E' `shouldBe` 124);
				it "latin small letter r with tail to IPA Number 125"
					(unicodeToNumber 'ɽ' `shouldBe` 125);
				it "latin small letter r with tail code point to IPA Number 125"
					(unicodeToNumber '\x027D' `shouldBe` 125);
				it "latin small letter phi to IPA Number 126"
					(unicodeToNumber 'ɸ' `shouldBe` 126);
				it "latin small letter phi code point to IPA Number 126"
					(unicodeToNumber '\x0278' `shouldBe` 126);
				it "greek small letter beta to IPA Number 127"
					(unicodeToNumber 'β' `shouldBe` 127);
				it "latin small letter beta code point to IPA Number 127"
					(unicodeToNumber '\x03B2' `shouldBe` 127);
				it "latin small letter f to IPA Number 128"
					(unicodeToNumber 'f' `shouldBe` 128);
				it "latin small letter f code point to IPA Number 128"
					(unicodeToNumber '\x0066' `shouldBe` 128);
				it "latin small letter v to IPA Number 129"
					(unicodeToNumber 'v' `shouldBe` 129);
				it "latin small letter v code point to IPA Number 129"
					(unicodeToNumber '\x0076' `shouldBe` 129);
				it "greek small letter theta to IPA Number 130"
					(unicodeToNumber 'θ' `shouldBe` 130);
				it "greek small letter theta code point to IPA Number 130"
					(unicodeToNumber '\x03B8' `shouldBe` 130);
				it "latin small letter eth to IPA Number 131"
					(unicodeToNumber 'ð' `shouldBe` 131);
				it "latin small letter eth code point to IPA Number 131"
					(unicodeToNumber '\x00F0' `shouldBe` 131);
				it "latin small letter s to IPA Number 132"
					(unicodeToNumber 's' `shouldBe` 132);
				it "latin small letter s code point to IPA Number 132"
					(unicodeToNumber '\x0073' `shouldBe` 132);
				it "latin small letter z to IPA Number 133"
					(unicodeToNumber 'z' `shouldBe` 133);
				it "latin small letter z code point to IPA Number 133"
					(unicodeToNumber '\x007A' `shouldBe` 133);
				it "latin small letter esh to IPA Number 134"
					(unicodeToNumber 'ʃ' `shouldBe` 134);
				it "latin small letter esh code point to IPA Number 134"
					(unicodeToNumber '\x0283' `shouldBe` 134);
				it "latin small letter ezh to IPA Number 135"
					(unicodeToNumber 'ʒ' `shouldBe` 135);
				it "latin small letter ezh code point to IPA Number 135"
					(unicodeToNumber '\x0292' `shouldBe` 135);
				it "latin small letter s with hook to IPA Number 136"
					(unicodeToNumber 'ʂ' `shouldBe` 136);
				it "latin small letter s with hook code point to IPA Number 136"
					(unicodeToNumber '\x0282' `shouldBe` 136);
				it "latin small letter z with retroflex hook to IPA Number 137"
					(unicodeToNumber 'ʐ' `shouldBe` 137);
				it "latin small letter z with retroflex hook code point to IPA Number 137"
					(unicodeToNumber '\x0290' `shouldBe` 137);
				it "latin small letter c with cedilla to IPA Number 138"
					(unicodeToNumber 'ç' `shouldBe` 138);
				it "latin small letter c with cedilla code point to IPA Number 138"
					(unicodeToNumber '\x00E7' `shouldBe` 138);
				it "latin small letter j with crossed tail to IPA Number 139"
					(unicodeToNumber 'ʝ' `shouldBe` 139);
				it "latin small letter j with crossed tail code point to IPA Number 139"
					(unicodeToNumber '\x029D' `shouldBe` 139);
				it "latin small letter x to IPA Number 140"
					(unicodeToNumber 'x' `shouldBe` 140);
				it "latin small letter x code point to IPA Number 140"
					(unicodeToNumber '\x0078' `shouldBe` 140);
				it "latin small letter gamma to IPA Number 141"
					(unicodeToNumber 'ɣ' `shouldBe` 141);
				it "latin small letter gamma code point to IPA Number 141"
					(unicodeToNumber '\x0263' `shouldBe` 141);
				it "greek small letter chi to IPA Number 142"
					(unicodeToNumber 'χ' `shouldBe` 142);
				it "greek small letter chi code point to IPA Number 142"
					(unicodeToNumber '\x03C7' `shouldBe` 142);
				it "latin letter small capital inverted r to IPA Number 143"
					(unicodeToNumber 'ʁ' `shouldBe` 143);
				it "latin letter small capital inverted r code point to IPA Number 143"
					(unicodeToNumber '\x0281' `shouldBe` 143);
				it "latin small letter h with stroke to IPA Number 144"
					(unicodeToNumber 'ħ' `shouldBe` 144);
				it "latin small letter h with stroke code point to IPA Number 144"
					(unicodeToNumber '\x0127' `shouldBe` 144);
				it "latin letter pharyngeal voiced fricative to IPA Number 145"
					(unicodeToNumber 'ʕ' `shouldBe` 145);
				it "latin letter pharyngeal voiced fricative code point to IPA Number 145"
					(unicodeToNumber '\x0295' `shouldBe` 145);
				it "latin small letter h to IPA Number 146"
					(unicodeToNumber 'h' `shouldBe` 146);
				it "latin small letter h code point to IPA Number 146"
					(unicodeToNumber '\x0068' `shouldBe` 146);
				it "latin small letter h with hook to IPA Number 147"
					(unicodeToNumber 'ɦ' `shouldBe` 147);
				it "latin small letter h with hook code point to IPA Number 147"
					(unicodeToNumber '\x0266' `shouldBe` 147);
				it "latin small letter l with belt to IPA Number 148"
					(unicodeToNumber 'ɬ' `shouldBe` 148);
				it "latin small letter l with belt code point to IPA Number 148"
					(unicodeToNumber '\x026C' `shouldBe` 148);
				it "latin small letter lezh to IPA Number 149"
					(unicodeToNumber 'ɮ' `shouldBe` 149);
				it "latin small letter lezh code point to IPA Number 149"
					(unicodeToNumber '\x026E' `shouldBe` 149);
				it "latin small letter v with hook to IPA Number 150"
					(unicodeToNumber 'ʋ' `shouldBe` 150);
				it "latin small letter v with hook code point to IPA Number 150"
					(unicodeToNumber '\x028B' `shouldBe` 150);
				it "latin small letter turned r to IPA Number 151"
					(unicodeToNumber 'ɹ' `shouldBe` 151);
				it "latin small letter turned r code point to IPA Number 151"
					(unicodeToNumber '\x0279' `shouldBe` 151);
				it "latin small letter turned r with hook to IPA Number 152"
					(unicodeToNumber 'ɻ' `shouldBe` 152);
				it "latin small letter turned r with hook code point to IPA Number 152"
					(unicodeToNumber '\x027B' `shouldBe` 152);
				it "latin small letter j to IPA Number 153"
					(unicodeToNumber 'j' `shouldBe` 153);
				it "latin small letter j code point to IPA Number 153"
					(unicodeToNumber '\x006A' `shouldBe` 153);
				it "latin small letter turned m with long leg to IPA Number 154"
					(unicodeToNumber 'ɰ' `shouldBe` 154);
				it "latin small letter turned m with long leg code point to IPA Number 154"
					(unicodeToNumber '\x0270' `shouldBe` 154);
				it "latin small letter l to IPA Number 155"
					(unicodeToNumber 'l' `shouldBe` 155);
				it "latin small letter l code point to IPA Number 155"
					(unicodeToNumber '\x006C' `shouldBe` 155);
				it "latin small letter l with retroflex hook to IPA Number 156"
					(unicodeToNumber 'ɭ' `shouldBe` 156);
				it "latin small letter l with retroflex hook code point to IPA Number 156"
					(unicodeToNumber '\x026D' `shouldBe` 156);
				it "latin small letter turned y to IPA Number 157"
					(unicodeToNumber 'ʎ' `shouldBe` 157);
				it "latin small letter turned y code point to IPA Number 157"
					(unicodeToNumber '\x028E' `shouldBe` 157);
				it "latin letter small capital l to IPA Number 158"
					(unicodeToNumber 'ʟ' `shouldBe` 158);
				it "latin letter small capital l code point to IPA Number 158"
					(unicodeToNumber '\x029F' `shouldBe` 158);
				it "latin letter bilabial click to IPA Number 176"
					(unicodeToNumber 'ʘ' `shouldBe` 176);
				it "latin letter bilabial click code point to IPA Number 176"
					(unicodeToNumber '\x0298' `shouldBe` 176);
				it "latin letter dental click to IPA Number 177"
					(unicodeToNumber 'ǀ' `shouldBe` 177);
				it "latin letter dental click code point"
					(unicodeToNumber '\x01C0' `shouldBe` 177);
				it "latin letter retroflex click to IPA Number 178"
					(unicodeToNumber 'ǃ' `shouldBe` 178);
				it "latin letter retroflex click"
					(unicodeToNumber '\x01C3' `shouldBe` 178);
				it "latin letter alveolar click to IPA Number 179"
					(unicodeToNumber 'ǂ' `shouldBe` 179);
				it "latin letter alveolar click code point"
					(unicodeToNumber '\x01C2' `shouldBe` 179);
				it "latin letter lateral click to IPA Number 180"
					(unicodeToNumber 'ǁ' `shouldBe` 180);
				it "latin letter lateral click code point"
					(unicodeToNumber '\x01C1' `shouldBe` 180);
				it "latin small letter b with hook to IPA Number 160"
					(unicodeToNumber 'ɓ' `shouldBe` 160);
				it "latin small letter b with hook code point"
					(unicodeToNumber '\x0253' `shouldBe` 160);
				it "latin small letter d with hook to IPA Number 162"
					(unicodeToNumber 'ɗ' `shouldBe` 162);
				it "latin small letter d with hook code point"
					(unicodeToNumber '\x0257' `shouldBe` 162);
				it "latin small letter dotless j with stroke and hook to IPA Number 164"
					(unicodeToNumber 'ʄ' `shouldBe` 164);
				it "latin small letter dotless j with stroke and hook code point"
					(unicodeToNumber '\x0284' `shouldBe` 164);
				it "latin small letter g with hook to IPA Number 166"
					(unicodeToNumber 'ɠ' `shouldBe` 166);
				it "latin small letter g with hook code point"
					(unicodeToNumber '\x0260' `shouldBe` 166);
				it "latin letter small capital g with hook to IPA Number 168"
					(unicodeToNumber 'ʛ' `shouldBe` 168);
				it "latin letter small capital g with hook code point"
					(unicodeToNumber '\x029B' `shouldBe` 168);
				it "modifier letter apostrophe to IPA Number 401"
					(unicodeToNumber 'ʼ' `shouldBe` 401);
				it "modifier letter apostrophe code point"
					(unicodeToNumber '\x02BC' `shouldBe` 401);
				it "latin small letter i to IPA Number 301"
					(unicodeToNumber 'i' `shouldBe` 301);
				it "latin small letter i code point"
					(unicodeToNumber '\x0069' `shouldBe` 301);
				it "latin small letter y to IPA Number 309"
					(unicodeToNumber 'y' `shouldBe` 309);
				it "latin small letter y code point"
					(unicodeToNumber '\x0079' `shouldBe` 309);
				it "latin small letter i with stroke to IPA Number 317"
					(unicodeToNumber 'ɨ' `shouldBe` 317);
				it "latin small letter i with stroke code point"
					(unicodeToNumber '\x0268' `shouldBe` 317);
				it "latin small letter u bar to IPA Number 318"
					(unicodeToNumber 'ʉ' `shouldBe` 318);
				it "latin small letter u bar code point"
					(unicodeToNumber '\x0289' `shouldBe` 318);
				it "latin small letter turned m to IPA Number 316"
					(unicodeToNumber 'ɯ' `shouldBe` 316);
				it "latin small letter turned m code point"
					(unicodeToNumber '\x026F' `shouldBe` 316);
				it "latin small letter u to IPA Number 308"
					(unicodeToNumber 'u' `shouldBe` 308);
				it "latin small letter u code point"
					(unicodeToNumber '\x0075' `shouldBe` 308);
				it "latin letter small capital i to IPA Number 319"
					(unicodeToNumber 'ɪ' `shouldBe` 319);
				it "latin letter small capital i code paint"
					(unicodeToNumber '\x026A' `shouldBe` 319);
				it "latin letter small capital y to IPA Number 320"
					(unicodeToNumber 'ʏ' `shouldBe` 320);
				it "latin letter small capital y code point"
					(unicodeToNumber '\x028F' `shouldBe` 320);
				it "latin small letter upsilon to IPA Number 321"
					(unicodeToNumber 'ʊ' `shouldBe` 321);
				it "latin small letter upsilon code point"
					(unicodeToNumber '\x028A' `shouldBe` 321);
				it "latin small letter e to IPA Number 302"
					(unicodeToNumber 'e' `shouldBe` 302);
				it "latin small letter e code point"
					(unicodeToNumber '\x0065' `shouldBe` 302);
				it "latin small letter o with stroke to IPA Number 310"
					(unicodeToNumber 'ø' `shouldBe` 310);
				it "latin small letter o with stroke code point"
					(unicodeToNumber '\x00F8' `shouldBe` 310);
				it "latin small letter reversed e to IPA Number 397"
					(unicodeToNumber 'ɘ' `shouldBe` 397);
				it "latin small letter reversed e code point"
					(unicodeToNumber '\x0258' `shouldBe` 397);
				it "latin small letter barred o to IPA Number 323"
					(unicodeToNumber 'ɵ' `shouldBe` 323);
				it "latin small letter barred o code point"
					(unicodeToNumber '\x0275' `shouldBe` 323);
				it "latin small letter rams horn to IPA Number 315"
					(unicodeToNumber 'ɤ' `shouldBe` 315);
				it "latin small letter rams horn code point"
					(unicodeToNumber '\x0264' `shouldBe` 315);
				it "latin small letter o to IPA Number 307"
					(unicodeToNumber 'o' `shouldBe` 307);
				it "latin small letter o code point"
					(unicodeToNumber '\x006F' `shouldBe` 307);
				it "latin small letter schwa to IPA Number 322"
					(unicodeToNumber 'ə' `shouldBe` 322);
				it "latin small letter schwa code point"
					(unicodeToNumber '\x0259' `shouldBe` 322);
				it "latin small letter open e to IPA Number 303"
					(unicodeToNumber 'ɛ' `shouldBe` 303);
				it "latin small letter open e code point"
					(unicodeToNumber '\x025B' `shouldBe` 303);
				it "latin small ligature oe to IPA Number 311"
					(unicodeToNumber 'œ' `shouldBe` 311);
				it "latin small ligature oe code point"
					(unicodeToNumber '\x0153' `shouldBe` 311);
				it "latin small letter reversed open e to IPA Number 326"
					(unicodeToNumber 'ɜ' `shouldBe` 326);
				it "latin small letter reversed open e code point"
					(unicodeToNumber '\x025C' `shouldBe` 326);
				it "latin small letter closed reversed open e to IPA Number 395"
					(unicodeToNumber 'ɞ' `shouldBe` 395);
				it "latin small letter closed reversed open e code point"
					(unicodeToNumber '\x025E' `shouldBe` 395);
				it "latin small letter turned v to IPA Number 314"
					(unicodeToNumber 'ʌ' `shouldBe` 314);
				it "latin small letter turned v code point"
					(unicodeToNumber '\x028C' `shouldBe` 314);
				it "latin small letter open o to IPA Number 306"
					(unicodeToNumber 'ɔ' `shouldBe` 306);
				it "latin small letter open o code point"
					(unicodeToNumber '\x0254' `shouldBe` 306);
				it "latin small letter ae to IPA Number 325"
					(unicodeToNumber 'æ' `shouldBe` 325);
				it "latin small letter ae code point"
					(unicodeToNumber '\x00E6' `shouldBe` 325);
				it "latin small letter turned a to IPA Number 324"
					(unicodeToNumber 'ɐ' `shouldBe` 324);
				it "latin small letter turned a code point"
					(unicodeToNumber '\x0250' `shouldBe` 324);
				it "latin small letter a to IPA Number 304"
					(unicodeToNumber 'a' `shouldBe` 304);
				it "latin small letter a code point"
					(unicodeToNumber '\x0061' `shouldBe` 304);
				it "latin letter small capital oe to IPA Number 312"
					(unicodeToNumber 'ɶ' `shouldBe` 312);
				it "latin letter small capital oe code point"
					(unicodeToNumber '\x0276' `shouldBe` 312);
				it "latin small letter alpha to IPA Number 305"
					(unicodeToNumber 'ɑ' `shouldBe` 305);
				it "latin small letter alpha code point"
					(unicodeToNumber '\x0251' `shouldBe` 305);
				it "latin small letter turned alpha to IPA Number 313"
					(unicodeToNumber 'ɒ' `shouldBe` 313);
				it "latin small letter turned alpha code point"
					(unicodeToNumber '\x0252' `shouldBe` 313);
				it "latin small letter turned w to IPA Number 169"
					(unicodeToNumber 'ʍ' `shouldBe` 169);
				it "latin small letter turned w code point"
					(unicodeToNumber '\x028D' `shouldBe` 169);
				it "latin small letter w to IPA Number 170"
					(unicodeToNumber 'w' `shouldBe` 170);
				it "latin small letter w code point"
					(unicodeToNumber '\x0077' `shouldBe` 170);
				it "latin small letter turned h to IPA Number 171"
					(unicodeToNumber 'ɥ' `shouldBe` 171);
				it "latin small letter turned h code point"
					(unicodeToNumber '\x0265' `shouldBe` 171);
				it "latin letter small capital h to IPA Number 172"
					(unicodeToNumber 'ʜ' `shouldBe` 172);
				it "latin letter small capital h code point"
					(unicodeToNumber '\x029C' `shouldBe` 172);
				it "latin letter reversed glottal stop with stroke to IPA Number 174"
					(unicodeToNumber 'ʢ' `shouldBe` 174);
				it "latin letter reversed glottal stop with stroke code point"
					(unicodeToNumber '\x02A2' `shouldBe` 174);
				it "latin letter glottal stop with stroke to IPA Number 173"
					(unicodeToNumber 'ʡ' `shouldBe` 173);
				it "latin letter glottal stop with stroke code point"
					(unicodeToNumber '\x02A1' `shouldBe` 173);
				it "latin small letter c with curl to IPA Number 182"
					(unicodeToNumber 'ɕ' `shouldBe` 182);
				it "latin small letter c with curl code point"
					(unicodeToNumber '\x0255' `shouldBe` 182);
				it "latin small letter z with curl to IPA Number 183"
					(unicodeToNumber 'ʑ' `shouldBe` 183);
				it "latin small letter z with curl code point"
					(unicodeToNumber '\x0291' `shouldBe` 183);
				it "latin small letter turned r with long leg to IPA Number 181"
					(unicodeToNumber 'ɺ' `shouldBe` 181);
				it "latin small letter turned r with long leg code point"
					(unicodeToNumber '\x027A' `shouldBe` 181);
				it "latin small letter heng with hook to IPA Number 175"
					(unicodeToNumber 'ɧ' `shouldBe` 175);
				it "latin small letter heng with hook code point"
					(unicodeToNumber '\x0267' `shouldBe` 175);
				it "combining double inverted breve to IPA Number 433"
					(unicodeToNumber '͡' `shouldBe` 433);
				it "combining double inverted breve code point"
					(unicodeToNumber '\x0361' `shouldBe` 433);
				it "combining double breve below to IPA Number 433"
					(unicodeToNumber '͜' `shouldBe` 433);
				it "combining double breve below code point"
					(unicodeToNumber '\x035C' `shouldBe` 433);
				it "modifier letter vertical line to IPA Number 501"
					(unicodeToNumber 'ˈ' `shouldBe` 501);
				it "modifier letter vertical line code point"
					(unicodeToNumber '\x02C8' `shouldBe` 501);
				it "modifier letter low vertical line to IPA Number 502"
					(unicodeToNumber 'ˌ' `shouldBe` 502);
				it "modifier letter low vertical line code point"
					(unicodeToNumber '\x02CC' `shouldBe` 502);
				it "modifier letter triangular colon to IPA Number 503"
					(unicodeToNumber 'ː' `shouldBe` 503);
				it "modifier letter triangular colon code point"
					(unicodeToNumber '\x02D0' `shouldBe` 503);
				it "modifier letter half triangular colon to IPA Number 504"
					(unicodeToNumber 'ˑ' `shouldBe` 504);
				it "modifier letter half triangular colon code point"
					(unicodeToNumber '\x02D1' `shouldBe` 504);
				it "combining breve to IPA Number 505"
					(unicodeToNumber '̆' `shouldBe` 505);
				it "combining breve code point"
					(unicodeToNumber '\x0306' `shouldBe` 505);
				it "vertical line to IPA Number 507"
					(unicodeToNumber '|' `shouldBe` 507);
				it "vertical line code point"
					(unicodeToNumber '\x007C' `shouldBe` 507);
				it "double vertical line to IPA Number 508"
					(unicodeToNumber '‖' `shouldBe` 508);
				it "double vertical line code point"
					(unicodeToNumber '\x2016' `shouldBe` 508);
				it "full stop to IPA Number 506"
					(unicodeToNumber '.' `shouldBe` 506);
				it "full stop code point"
					(unicodeToNumber '\x002E' `shouldBe` 506);
				it "undertie to IPA Number 509"
					(unicodeToNumber '‿' `shouldBe` 509);
				it "undertie code point"
					(unicodeToNumber '\x203F' `shouldBe` 509);
				it "combining ring below to IPA Number 402"
					(unicodeToNumber '̥' `shouldBe` 402);
				it "combining ring below code point"
					(unicodeToNumber '\x0325' `shouldBe` 402);
				it "combining ring above to IPA Number 402"
					(unicodeToNumber '̊' `shouldBe` 402);
				it "combining ring above code point"
					(unicodeToNumber '\x030A' `shouldBe` 402);


				it "combining caron below to IPA Number 403"
					(unicodeToNumber '̬' `shouldBe` 403);
				it "combining caron below code point"
					(unicodeToNumber '\x032C' `shouldBe` 403);
				it "modifier letter small h to IPA Number 404"
					(unicodeToNumber 'ʰ' `shouldBe` 404);
				it "modifier letter small h code point"
					(unicodeToNumber '\x02B0' `shouldBe` 404);
				it "combining right half ring below to IPA Number 411"
					(unicodeToNumber '̹' `shouldBe` 411);
				it "combining right half ring below code point"
					(unicodeToNumber '\x0339' `shouldBe` 411);
				it "combining left half ring below to IPA Number 412"
					(unicodeToNumber '̜' `shouldBe` 412);
				it "combining left half ring below code point"
					(unicodeToNumber '\x031C' `shouldBe` 412);
				it "combining plus sign below to IPA Number 413"
					(unicodeToNumber '̟' `shouldBe` 413);
				it "combining plus sign below"
					(unicodeToNumber '\x031F' `shouldBe` 413);
				it "combining minus sign below to IPA Number 414"
					(unicodeToNumber '̠' `shouldBe` 414);
				it "combining minus sign below code point"
					(unicodeToNumber '\x0320' `shouldBe` 414);
				it "combining diaeresis to IPA Number 415"
					(unicodeToNumber '̈' `shouldBe` 415);
				it "combining diaeresis code point"
					(unicodeToNumber '\x0308' `shouldBe` 415);
				it "combining x above to IPA Number 416"
					(unicodeToNumber '̽' `shouldBe` 416);
				it "combining x above code point"
					(unicodeToNumber '\x033D' `shouldBe` 416);

				it "combining vertical line above to IPA Number 431"
					(unicodeToNumber '̍' `shouldBe` 431);
				it "combining vertical line above code point"
					(unicodeToNumber '\x030D' `shouldBe` 431);
				it "combining vertical line below to IPA Number 431"
					(unicodeToNumber '̩' `shouldBe` 431);
				it "combining vertical line below code point"
					(unicodeToNumber '\x0329' `shouldBe` 431);
				it "combining inverted breve below to IPA Number 432"
					(unicodeToNumber '̯' `shouldBe` 432);
				it "combining inverted breve below code point"
					(unicodeToNumber '\x032F' `shouldBe` 432);
				it "modifier letter rhotic hook to IPA Number 419"
					(unicodeToNumber '˞' `shouldBe` 419);
				it "modifier letter rhotic hook code point"
					(unicodeToNumber '\x02DE' `shouldBe` 419);
				it "combining diaeresis below to IPA Number 405"
					(unicodeToNumber '̤' `shouldBe` 405);
				it "combining diaeresis below code point"
					(unicodeToNumber '\x0324' `shouldBe` 405);
				it "combining tilde below to IPA Number 406"
					(unicodeToNumber '̰' `shouldBe` 406);
				it "combining tilde below code point"
					(unicodeToNumber '\x0330' `shouldBe` 406);
				it "combining seagul below to IPA Number 407"
					(unicodeToNumber '̼' `shouldBe` 407);
				it "combining seagul below code point"
					(unicodeToNumber '\x033C' `shouldBe` 407);
				it "modifier letter small w to IPA Number 420"
					(unicodeToNumber 'ʷ' `shouldBe` 420);
				it "modifier letter small w code point"
					(unicodeToNumber '\x02B7' `shouldBe` 420);
				it "modifier letter small j to IPA Number 421"
					(unicodeToNumber 'ʲ' `shouldBe` 421);
				it "modifier letter small j code point"
					(unicodeToNumber '\x02B2' `shouldBe` 421);
				it "modifier letter small gamma to IPA Number 422"
					(unicodeToNumber 'ˠ' `shouldBe` 422);
				it "modifier letter small gamma code point"
					(unicodeToNumber '\x02E0' `shouldBe` 422);
				it "modifier letter small reversed glottal stop to IPA Number 423"
					(unicodeToNumber 'ˤ' `shouldBe` 423);
				it "modifier letter small reversed glottal stop code point"
					(unicodeToNumber '\x02E4' `shouldBe` 423);
				it "combining tilde overlay to IPA Number 428"
					(unicodeToNumber '̴' `shouldBe` 428);
				it "combining tilde overlay code point"
					(unicodeToNumber '\x0334' `shouldBe` 428);
				it "combining up tack below to IPA Number 429"
					(unicodeToNumber '̝' `shouldBe` 429);
				it "combining up tack below code point"
					(unicodeToNumber '\x031D' `shouldBe` 429);
				it "combining down tack below to IPA Number 430"
					(unicodeToNumber '̞' `shouldBe` 430);
				it "combining down tack below code point"
					(unicodeToNumber '\x031E' `shouldBe` 430);
				it "combining left tack below to IPA Number 417"
					(unicodeToNumber '̘' `shouldBe` 417);
				it "combining left tack below code point"
					(unicodeToNumber '\x0318' `shouldBe` 417);
				it "combining right tack below to IPA Number 418"
					(unicodeToNumber '̙' `shouldBe` 418);
				it "combining right tack below code point"
					(unicodeToNumber '\x0319' `shouldBe` 418);
				it "combining bridge below to IPA Number 408"
					(unicodeToNumber '̪' `shouldBe` 408);
				it "combining bridge below code point"
					(unicodeToNumber '\x032A' `shouldBe` 408);
				it "combining inverted bridge below to IPA Number 409"
					(unicodeToNumber '̺' `shouldBe` 409);
				it "combining inverted bridge below code point"
					(unicodeToNumber '\x033A' `shouldBe` 409);
				it "combining square below to IPA Number 410"
					(unicodeToNumber '̻' `shouldBe` 410);
				it "combining square below code point"
					(unicodeToNumber '\x033B' `shouldBe` 410);
				it "combining tilde to IPA Number 424"
					(unicodeToNumber '̃' `shouldBe` 424);
				it "combining tilde code point"
					(unicodeToNumber '\x0303' `shouldBe` 424);
				it "superscript latin small letter n to IPA Number 425"
					(unicodeToNumber 'ⁿ' `shouldBe` 425);
				it "superscript latin small letter n code point"
					(unicodeToNumber '\x207F' `shouldBe` 425);
				it "modifier letter small l to IPA Number 426"
					(unicodeToNumber 'ˡ' `shouldBe` 426);
				it "modifier letter small l code point"
					(unicodeToNumber '\x02E1' `shouldBe` 426);
				it "combining left angle above to IPA Number 427"
					(unicodeToNumber '̚' `shouldBe` 427);
				it "combining left angle above code point"
					(unicodeToNumber '\x031A' `shouldBe` 427);
				it "combining double acute accent to IPA Number 512"
					(unicodeToNumber '̋' `shouldBe` 512);
				it "combining double acute accent code point"
					(unicodeToNumber '\x030B' `shouldBe` 512);
				it "combining acute accent to IPA Number 513"
					(unicodeToNumber '́' `shouldBe` 513);
				it "combining acute accent code point"
					(unicodeToNumber '\x0301' `shouldBe` 513);
				it "combining macron to IPA Number 514"
					(unicodeToNumber '̄' `shouldBe` 514);
				it "combining macron code point"
					(unicodeToNumber '\x0304' `shouldBe` 514);
				it "combining grave accent to IPA Number 515"
					(unicodeToNumber '̀' `shouldBe` 515);
				it "combining grave accent code point"
					(unicodeToNumber '\x0300' `shouldBe` 515);
				it "combining double grave accent to IPA Number 516"
					(unicodeToNumber '̏' `shouldBe` 516);
				it "combining double grave accent code point"
					(unicodeToNumber '\x030F' `shouldBe` 516);
				it "downwards arrow to IPA Number 517"
					(unicodeToNumber '↓' `shouldBe` 517);
				it "downwards arrow code point"
					(unicodeToNumber '\x2193' `shouldBe` 517);
				it "combining caron to IPA Number 524"
					(unicodeToNumber '̌' `shouldBe` 524);
				it "combining caron code point"
					(unicodeToNumber '\x030C' `shouldBe` 524);
				it "combining circumflex accent to IPA Number 525"
					(unicodeToNumber '̂' `shouldBe` 525);
				it "combining circumflex accent code point"
					(unicodeToNumber '\x0302' `shouldBe` 525);
				it "combining macron acute to IPA Number 526"
					(unicodeToNumber '᷄' `shouldBe` 526);
				it "combining macron acute code point"
					(unicodeToNumber '\x1DC4' `shouldBe` 526);
				it "combining grave macron to IPA Number 527"
					(unicodeToNumber '᷅' `shouldBe` 527);
				it "combining grave macron code point"
					(unicodeToNumber '\x1DC5' `shouldBe` 527);
				it "combining grave acute grave to IPA Number 528" 
					(unicodeToNumber '᷈' `shouldBe` 528);
				it "combining grave acute grave code point" 
					(unicodeToNumber '\x1DC8' `shouldBe` 528);
				it "north east arrow to IPA Number 510"
					(unicodeToNumber '↗' `shouldBe` 510);
				it "north east arrow code point"
					(unicodeToNumber '\x2197' `shouldBe` 510);
				it "south east arrow to IPA Number 511"
					(unicodeToNumber '↘' `shouldBe` 511);
				it "south east arrow code point"
					(unicodeToNumber '\x2198' `shouldBe` 511);
				it "downwards arrow to IPA Number 517"
					(unicodeToNumber '↓' `shouldBe` 517);
				it "downwards arrow code point"
					(unicodeToNumber '\x2193' `shouldBe` 517);
				it "upwards arrow to IPA Number 518"
					(unicodeToNumber '↑' `shouldBe` 518);
				it "upwards arrow code point"
					(unicodeToNumber '\x2191' `shouldBe` 518);
				it "modifier letter extra high tone bar to IPA Number 519"
					(unicodeToNumber '˥' `shouldBe` 519);
				it "modifier letter extra high tone bar code point"
					(unicodeToNumber '\x02E5' `shouldBe` 519);
				it "modifier letter high tone bar to IPA Number 520"
					(unicodeToNumber '˦' `shouldBe` 520);
				it "modifier letter high tone bar code point"
					(unicodeToNumber '\x02E6' `shouldBe` 520);
				it "modifier letter mid tone bar to IPA Number 521"
					(unicodeToNumber '˧' `shouldBe` 521);
				it "modifier letter mid tone bar code point"
					(unicodeToNumber '\x02E7' `shouldBe` 521);
				it "modifier letter low tone bar to IPA Number 522"
					(unicodeToNumber '˨' `shouldBe` 522);
				it "modifier letter low tone bar code point"
					(unicodeToNumber '\x02E8' `shouldBe` 522);
				it "modifier letter extra low tone bar to IPA Number 523"
					(unicodeToNumber '˩' `shouldBe` 523);
				it "modifier letter extra low tone bar"
					(unicodeToNumber '\x02E9' `shouldBe` 523);
			}
		)