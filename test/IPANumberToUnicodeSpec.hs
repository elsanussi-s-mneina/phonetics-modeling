module IPANumberToUnicodeSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import IPANumberToUnicode (numberToUnicode)

numberToUnicodeSpec :: Spec
numberToUnicodeSpec =
	describe "IPA Numbers to Convert Unicode code points"
		(
			do
			{
				it "IPA Number 101 to latin small letter p"
					(numberToUnicode 101 `shouldBe` 'p');
				it "IPA Number 101 to latin small letter p code point"
					(numberToUnicode 101 `shouldBe` '\x0070');
				it "IPA Number 102 to latin small letter b"
					(numberToUnicode 102 `shouldBe` 'b');
				it "IPA Number 102 to latin small letter b code point"
					(numberToUnicode 102 `shouldBe` '\x0062');
				it "IPA Number 103 to latin small letter t"
					(numberToUnicode 103 `shouldBe` 't');
				it "IPA Number 103 to latin small letter t code point"
					(numberToUnicode 103 `shouldBe` '\x0074');
				it "IPA Number 104 to latin small letter d"
					(numberToUnicode 104  `shouldBe` 'd');
				it "IPA Number 104 to latin small letter d code point"
					(numberToUnicode 104 `shouldBe` '\x0064');
				it "IPA Number 105 to latin small letter t with retroflex hook"
					(numberToUnicode 105 `shouldBe` 'ʈ');
				it "IPA Number 105 to latin small letter t with retroflex hook code point"
					(numberToUnicode 105 `shouldBe` '\x0288');
				it "IPA Number 106 to latin small letter d with tail"
					(numberToUnicode 106 `shouldBe` 'ɖ');
				it "IPA Number 106 to latin small letter d with tail code point"
					(numberToUnicode 106 `shouldBe` '\x0256');
				it "IPA Number 107 to latin small letter c"
					(numberToUnicode 107 `shouldBe` 'c');
				it "IPA Number 107 to latin small letter c code point"
					(numberToUnicode 107 `shouldBe` '\x0063');
				it "IPA Number 108 to latin small dotless j with stroke"
					(numberToUnicode 108 `shouldBe` 'ɟ');
				it "IPA Number 108 to latin small dotless j with stroke code point"
					(numberToUnicode 108 `shouldBe` '\x025F');
				it "IPA Number 109 to latin small letter k"
					(numberToUnicode 109 `shouldBe` 'k');
				it "IPA Number 109 to latin small letter k code point"
					(numberToUnicode 109 `shouldBe` '\x006B');
				it "IPA Number 110 to latin small letter g"
					(numberToUnicode 110 `shouldBe` 'g');
				it "IPA Number 110 to latin small letter g code point"
					(numberToUnicode 110 `shouldBe` '\x0067');
				it "IPA Number 111 to latin small letter q"
					(numberToUnicode 111 `shouldBe` 'q');
				it "IPA Number 111 to latin small letter q code point"
					(numberToUnicode 111 `shouldBe` '\x0071');
				it "IPA Number 112 to latin letter small capital g"
					(numberToUnicode 112 `shouldBe` 'ɢ');
				it "IPA Number 112 to latin letter small capital g code point"
					(numberToUnicode 112 `shouldBe` '\x0262');
				it "IPA Number 113 to latin letter glottal stop"
					(numberToUnicode 113 `shouldBe` 'ʔ');
				it "IPA Number 113 to latin letter glottal stop code point"
					(numberToUnicode 113 `shouldBe` '\x0294');
				it "IPA Number 114 to latin small letter m"
					(numberToUnicode 114 `shouldBe` 'm');
				it "IPA Number 114 to latin small letter m code point"
					(numberToUnicode 114 `shouldBe` '\x006D');
				it "IPA Number 115 to latin small letter m with hook"
					(numberToUnicode 115 `shouldBe` 'ɱ');
				it "IPA Number 115 to latin small letter m with hook code point"
					(numberToUnicode 115 `shouldBe` '\x0271');
				it "IPA Number 116 to latin small letter n"
					(numberToUnicode 116 `shouldBe` 'n');
				it "IPA Number 116 to latin small letter n code point"
					(numberToUnicode 116 `shouldBe` '\x006E');
				it "IPA Number 117 to latin small letter n with retroflex hook"
					(numberToUnicode 117 `shouldBe` 'ɳ');
				it "IPA Number 117 to latin small letter n with retroflex hook code point"
					(numberToUnicode 117 `shouldBe` '\x0273');
				it "IPA Number 118 to latin small letter n with left hook"
					(numberToUnicode 118 `shouldBe` 'ɲ');
				it "IPA Number 118 to latin small letter n with left hook code point"
					(numberToUnicode 118 `shouldBe` '\x0272');
				it "IPA Number 119 to latin small letter eng"
					(numberToUnicode 119 `shouldBe` 'ŋ');
				it "IPA Number 119 to latin small letter eng code point"
					(numberToUnicode 119 `shouldBe` '\x014B');
				it "IPA Number 120 to latin letter small capital n"
					(numberToUnicode 120 `shouldBe` 'ɴ');
				it "IPA Number 120 to latin letter small capital n code point"
					(numberToUnicode 120 `shouldBe` '\x0274');
				it "IPA Number 121 to latin letter small capital b"
					(numberToUnicode 121 `shouldBe` 'ʙ');
				it "IPA Number 121 to latin letter small capital b code point"
					(numberToUnicode 121 `shouldBe` '\x0299');
				it "IPA Number 122 to latin small letter r"
					(numberToUnicode 122 `shouldBe` 'r');
				it "IPA Number 122 to latin small letter r code point"
					(numberToUnicode 122 `shouldBe` '\x0072');
				it "IPA Number 123 to latin letter small capital r"
					(numberToUnicode 123 `shouldBe` 'ʀ');
				it "IPA Number 123 to latin letter small capital r code point"
					(numberToUnicode 123 `shouldBe` '\x0280');
				it "IPA Number 184 to latin small letter v with right hook"
					(numberToUnicode 184 `shouldBe` 'ⱱ');
				it "IPA Number 184 to latin small letter v with right hook code point"
					(numberToUnicode 184 `shouldBe` '\x2C71');
				it "IPA Number 124 to latin small letter r with fishhook"
					(numberToUnicode 124 `shouldBe` 'ɾ');
				it "IPA Number 124 to latin small letter r with fishhook code point"
					(numberToUnicode 124 `shouldBe` '\x027E');
				it "IPA Number 125 to latin small letter r with tail"
					(numberToUnicode 125 `shouldBe` 'ɽ');
				it "IPA Number 125 to latin small letter r with tail code point"
					(numberToUnicode 125 `shouldBe` '\x027D');
				it "IPA Number 126 to latin small letter phi"
					(numberToUnicode 126 `shouldBe` 'ɸ');
				it "IPA Number 126 to latin small letter phi code point"
					(numberToUnicode 126 `shouldBe` '\x0278');
				it "IPA Number 127 to greek small letter beta"
					(numberToUnicode 127 `shouldBe` 'β');
				it "IPA Number 127 to latin small letter beta code point"
					(numberToUnicode 127 `shouldBe` '\x03B2');
				it "IPA Number 128 to latin small letter f"
					(numberToUnicode 128 `shouldBe` 'f');
				it "IPA Number 128 to latin small letter f code point"
					(numberToUnicode 128 `shouldBe` '\x0066');
				it "IPA Number 129 to latin small letter v"
					(numberToUnicode 129 `shouldBe` 'v');
				it "IPA Number 129 to latin small letter v code point"
					(numberToUnicode 129 `shouldBe` '\x0076');
				it "IPA Number 130 to greek small letter theta"
					(numberToUnicode 130 `shouldBe` 'θ');
				it "IPA Number 130 to greek small letter theta code point"
					(numberToUnicode 130 `shouldBe` '\x03B8');
				it "IPA Number 131 to latin small letter eth"
					(numberToUnicode 131 `shouldBe` 'ð');
				it "IPA Number 131 to latin small letter eth code point"
					(numberToUnicode 131 `shouldBe` '\x00F0');
				it "IPA Number 132 to latin small letter s"
					(numberToUnicode 132 `shouldBe` 's');
				it "IPA Number 132 to latin small letter s code point"
					(numberToUnicode 132 `shouldBe` '\x0073');
				it "IPA Number 133 to latin small letter z"
					(numberToUnicode 133 `shouldBe` 'z');
				it "IPA Number 133 to latin small letter z code point"
					(numberToUnicode 133 `shouldBe` '\x007A');
				it "IPA Number 134 to latin small letter esh"
					(numberToUnicode 134 `shouldBe` 'ʃ');
				it "IPA Number 134 to latin small letter esh code point"
					(numberToUnicode 134 `shouldBe` '\x0283');
				it "IPA Number 135 to latin small letter ezh"
					(numberToUnicode 135 `shouldBe` 'ʒ');
				it "IPA Number 135 to latin small letter ezh code point"
					(numberToUnicode 135 `shouldBe` '\x0292');
				it "IPA Number 136 to latin small letter s with hook"
					(numberToUnicode 136 `shouldBe` 'ʂ');
				it "IPA Number 136 to latin small letter s with hook code point"
					(numberToUnicode 136 `shouldBe` '\x0282');
				it "IPA Number 137 to latin small letter z with retroflex hook"
					(numberToUnicode 137 `shouldBe` 'ʐ');
				it "IPA Number 137 to latin small letter z with retroflex hook code point"
					(numberToUnicode 137 `shouldBe` '\x0290');
				it "IPA Number 138 to latin small letter c with cedilla"
					(numberToUnicode 138 `shouldBe` 'ç');
				it "IPA Number 138 to latin small letter c with cedilla code point"
					(numberToUnicode 138 `shouldBe` '\x00E7');
				it "IPA Number 139 to latin small letter j with crossed tail"
					(numberToUnicode 139 `shouldBe` 'ʝ');
				it "IPA Number 139 to latin small letter j with crossed tail code point"
					(numberToUnicode 139 `shouldBe` '\x029D');
				it "IPA Number 140 to latin small letter x"
					(numberToUnicode 140 `shouldBe` 'x');
				it "IPA Number 140 to latin small letter x code point"
					(numberToUnicode 140 `shouldBe` '\x0078');
				it "IPA Number 141 to latin small letter gamma"
					(numberToUnicode 141 `shouldBe` 'ɣ');
				it "IPA Number 141 to latin small letter gamma code point"
					(numberToUnicode 141 `shouldBe` '\x0263');
				it "IPA Number 142 to greek small letter chi"
					(numberToUnicode 142 `shouldBe` 'χ');
				it "IPA Number 142 to greek small letter chi code point"
					(numberToUnicode 142 `shouldBe` '\x03C7');
				it "IPA Number 143 to latin letter small capital inverted r"
					(numberToUnicode 143 `shouldBe` 'ʁ');
				it "IPA Number 143 to latin letter small capital inverted r code point"
					(numberToUnicode 143 `shouldBe` '\x0281');
				it "IPA Number 144 to latin small letter h with stroke"
					(numberToUnicode 144 `shouldBe` 'ħ');
				it "IPA Number 144 to latin small letter h with stroke code point"
					(numberToUnicode 144 `shouldBe` '\x0127');
				it "IPA Number 145 to latin letter pharyngeal voiced fricative"
					(numberToUnicode 145 `shouldBe` 'ʕ');
				it "IPA Number 145 to latin letter pharyngeal voiced fricative code point"
					(numberToUnicode 145 `shouldBe` '\x0295');
				it "IPA Number 146 to latin small letter h"
					(numberToUnicode 146 `shouldBe` 'h');
				it "IPA Number 146 to latin small letter h code point"
					(numberToUnicode 146 `shouldBe` '\x0068');
				it "IPA Number 147 to latin small letter h with hook"
					(numberToUnicode 147 `shouldBe` 'ɦ');
				it "IPA Number 147 to latin small letter h with hook code point"
					(numberToUnicode 147 `shouldBe` '\x0266');
				it "IPA Number 148 to latin small letter l with belt"
					(numberToUnicode 148 `shouldBe` 'ɬ');
				it "IPA Number 148 to latin small letter l with belt code point"
					(numberToUnicode 148 `shouldBe` '\x026C');
				it "IPA Number 149 to latin small letter lezh"
					(numberToUnicode 149 `shouldBe` 'ɮ');
				it "IPA Number 149 to latin small letter lezh code point"
					(numberToUnicode 149 `shouldBe` '\x026E');
				it "IPA Number 150 to latin small letter v with hook"
					(numberToUnicode 150 `shouldBe` 'ʋ');
				it "IPA Number 150 to latin small letter v with hook code point"
					(numberToUnicode 150 `shouldBe` '\x028B');
				it "IPA Number 151 to latin small letter turned r"
					(numberToUnicode 151 `shouldBe` 'ɹ');
				it "IPA Number 151 to latin small letter turned r code point"
					(numberToUnicode 151 `shouldBe` '\x0279');
				it "IPA Number 152 to latin small letter turned r with hook"
					(numberToUnicode 152 `shouldBe` 'ɻ');
				it "IPA Number 152 to latin small letter turned r with hook code point"
					(numberToUnicode 152 `shouldBe` '\x027B');
				it "IPA Number 153 to latin small letter j"
					(numberToUnicode 153 `shouldBe` 'j');
				it "IPA Number 153 to latin small letter j code point"
					(numberToUnicode 153 `shouldBe` '\x006A');
				it "IPA Number 154 to latin small letter turned m with long leg"
					(numberToUnicode 154 `shouldBe` 'ɰ');
				it "IPA Number 154 to latin small letter turned m with long leg code point"
					(numberToUnicode 154 `shouldBe` '\x0270');
				it "IPA Number 155 to latin small letter l"
					(numberToUnicode 155 `shouldBe` 'l');
				it "IPA Number 155 to latin small letter l code point"
					(numberToUnicode 155 `shouldBe` '\x006C');
				it "IPA Number 156 to latin small letter l with retroflex hook"
					(numberToUnicode 156 `shouldBe` 'ɭ');
				it "IPA Number 156 to latin small letter l with retroflex hook code point"
					(numberToUnicode 156 `shouldBe` '\x026D');
				it "IPA Number 157 to latin small letter turned y"
					(numberToUnicode 157 `shouldBe` 'ʎ');
				it "IPA Number 157 to latin small letter turned y code point"
					(numberToUnicode 157 `shouldBe` '\x028E');
				it "IPA Number 158 to latin letter small capital l"
					(numberToUnicode 158 `shouldBe` 'ʟ');
				it "IPA Number 158 to latin letter small capital l code point"
					(numberToUnicode 158 `shouldBe` '\x029F');
				it "IPA Number 176 to latin letter bilabial click"
					(numberToUnicode 176 `shouldBe` 'ʘ');
				it "IPA Number 176 to latin letter bilabial click code point"
					(numberToUnicode 176 `shouldBe` '\x0298');
				it "IPA Number 177 to latin letter dental click"
					(numberToUnicode 177 `shouldBe` 'ǀ');
				it "latin letter dental click code point"
					(numberToUnicode 177 `shouldBe` '\x01C0');
				it "IPA Number 178 to latin letter retroflex click"
					(numberToUnicode 178 `shouldBe` 'ǃ');
				it "latin letter retroflex click"
					(numberToUnicode 178 `shouldBe` '\x01C3');
				it "IPA Number 179 to latin letter alveolar click"
					(numberToUnicode 179 `shouldBe` 'ǂ');
				it "latin letter alveolar click code point"
					(numberToUnicode 179 `shouldBe` '\x01C2');
				it "IPA Number 180 to latin letter lateral click"
					(numberToUnicode 180 `shouldBe` 'ǁ');
				it "latin letter lateral click code point"
					(numberToUnicode 180 `shouldBe` '\x01C1');
				it "IPA Number 160 to latin small letter b with hook"
					(numberToUnicode 160 `shouldBe` 'ɓ');
				it "latin small letter b with hook code point"
					(numberToUnicode 160 `shouldBe` '\x0253');
				it "IPA Number 162 to latin small letter d with hook"
					(numberToUnicode 162 `shouldBe` 'ɗ');
				it "latin small letter d with hook code point"
					(numberToUnicode 162 `shouldBe` '\x0257');
				it "IPA Number 164 to latin small letter dotless j with stroke and hook"
					(numberToUnicode 164 `shouldBe` 'ʄ');
				it "latin small letter dotless j with stroke and hook code point"
					(numberToUnicode 164 `shouldBe` '\x0284');
				it "IPA Number 166 to latin small letter g with hook"
					(numberToUnicode 166 `shouldBe` 'ɠ');
				it "latin small letter g with hook code point"
					(numberToUnicode 166 `shouldBe` '\x0260');
				it "IPA Number 168 to latin letter small capital g with hook"
					(numberToUnicode 168 `shouldBe` 'ʛ');
				it "latin letter small capital g with hook code point"
					(numberToUnicode 168 `shouldBe` '\x029B');
				it "IPA Number 401 to modifier letter apostrophe"
					(numberToUnicode 401 `shouldBe` 'ʼ');
				it "modifier letter apostrophe code point"
					(numberToUnicode 401 `shouldBe` '\x02BC');
				it "IPA Number 301 to latin small letter i"
					(numberToUnicode 301 `shouldBe` 'i');
				it "latin small letter i code point"
					(numberToUnicode 301 `shouldBe` '\x0069');
				it "IPA Number 309 to latin small letter y"
					(numberToUnicode 309 `shouldBe` 'y');
				it "latin small letter y code point"
					(numberToUnicode 309 `shouldBe` '\x0079');
				it "IPA Number 317 to latin small letter i with stroke"
					(numberToUnicode 317 `shouldBe` 'ɨ');
				it "latin small letter i with stroke code point"
					(numberToUnicode 317 `shouldBe` '\x0268');
				it "IPA Number 318 to latin small letter u bar"
					(numberToUnicode 318 `shouldBe` 'ʉ');
				it "latin small letter u bar code point"
					(numberToUnicode 318 `shouldBe` '\x0289');
				it "IPA Number 316 to latin small letter turned m"
					(numberToUnicode 316 `shouldBe` 'ɯ');
				it "latin small letter turned m code point"
					(numberToUnicode 316 `shouldBe` '\x026F');
				it "IPA Number 308 to latin small letter u"
					(numberToUnicode 308 `shouldBe` 'u');
				it "latin small letter u code point"
					(numberToUnicode 308 `shouldBe` '\x0075');
				it "IPA Number 319 to latin letter small capital i"
					(numberToUnicode 319 `shouldBe` 'ɪ');
				it "latin letter small capital i code paint"
					(numberToUnicode 319 `shouldBe` '\x026A');
				it "IPA Number 320 to latin letter small capital y"
					(numberToUnicode 320 `shouldBe` 'ʏ');
				it "latin letter small capital y code point"
					(numberToUnicode 320 `shouldBe` '\x028F');
				it "IPA Number 321 to latin small letter upsilon"
					(numberToUnicode 321 `shouldBe` 'ʊ');
				it "latin small letter upsilon code point"
					(numberToUnicode 321 `shouldBe` '\x028A');
				it "IPA Number 302 to latin small letter e"
					(numberToUnicode 302 `shouldBe` 'e');
				it "latin small letter e code point"
					(numberToUnicode 302 `shouldBe` '\x0065');
				it "IPA Number 310 to latin small letter o with stroke"
					(numberToUnicode 310 `shouldBe` 'ø');
				it "latin small letter o with stroke code point"
					(numberToUnicode 310 `shouldBe` '\x00F8');
				it "IPA Number 397 to latin small letter reversed e"
					(numberToUnicode 397 `shouldBe` 'ɘ');
				it "latin small letter reversed e code point"
					(numberToUnicode 397 `shouldBe` '\x0258');
				it "IPA Number 323 to latin small letter barred o"
					(numberToUnicode 323 `shouldBe` 'ɵ');
				it "latin small letter barred o code point"
					(numberToUnicode 323 `shouldBe` '\x0275');
				it "IPA Number 315 to latin small letter rams horn"
					(numberToUnicode 315 `shouldBe` 'ɤ');
				it "latin small letter rams horn code point"
					(numberToUnicode 315 `shouldBe` '\x0264');
				it "IPA Number 307 to latin small letter o"
					(numberToUnicode 307 `shouldBe` 'o');
				it "latin small letter o code point"
					(numberToUnicode 307 `shouldBe` '\x006F');
				it "IPA Number 322 to latin small letter schwa"
					(numberToUnicode 322 `shouldBe` 'ə');
				it "latin small letter schwa code point"
					(numberToUnicode 322 `shouldBe` '\x0259');
				it "IPA Number 303 to latin small letter open e"
					(numberToUnicode 303 `shouldBe` 'ɛ');
				it "latin small letter open e code point"
					(numberToUnicode 303 `shouldBe` '\x025B');
				it "IPA Number 311 to latin small ligature oe"
					(numberToUnicode 311 `shouldBe` 'œ');
				it "latin small ligature oe code point"
					(numberToUnicode 311 `shouldBe` '\x0153');
				it "IPA Number 326 to latin small letter reversed open e"
					(numberToUnicode 326 `shouldBe` 'ɜ');
				it "latin small letter reversed open e code point"
					(numberToUnicode 326 `shouldBe` '\x025C');
				it "IPA Number 395 to latin small letter closed reversed open e"
					(numberToUnicode 395 `shouldBe` 'ɞ');
				it "latin small letter closed reversed open e code point"
					(numberToUnicode 395 `shouldBe` '\x025E');
				it "IPA Number 314 to latin small letter turned v"
					(numberToUnicode 314 `shouldBe` 'ʌ');
				it "latin small letter turned v code point"
					(numberToUnicode 314 `shouldBe` '\x028C');
				it "IPA Number 306 to latin small letter open o"
					(numberToUnicode 306 `shouldBe` 'ɔ');
				it "latin small letter open o code point"
					(numberToUnicode 306 `shouldBe` '\x0254');
				it "IPA Number 325 to latin small letter ae"
					(numberToUnicode 325 `shouldBe` 'æ');
				it "latin small letter ae code point"
					(numberToUnicode 325 `shouldBe` '\x00E6');
				it "IPA Number 324 to latin small letter turned a"
					(numberToUnicode 324 `shouldBe` 'ɐ');
				it "latin small letter turned a code point"
					(numberToUnicode 324 `shouldBe` '\x0250');
				it "IPA Number 304 to latin small letter a"
					(numberToUnicode 304 `shouldBe` 'a');
				it "latin small letter a code point"
					(numberToUnicode 304 `shouldBe` '\x0061');
				it "IPA Number 312 to latin letter small capital oe"
					(numberToUnicode 312 `shouldBe` 'ɶ');
				it "latin letter small capital oe code point"
					(numberToUnicode 312 `shouldBe` '\x0276');
				it "IPA Number 305 to latin small letter alpha"
					(numberToUnicode 305 `shouldBe` 'ɑ');
				it "latin small letter alpha code point"
					(numberToUnicode 305 `shouldBe` '\x0251');
				it "IPA Number 313 to latin small letter turned alpha"
					(numberToUnicode 313 `shouldBe` 'ɒ');
				it "latin small letter turned alpha code point"
					(numberToUnicode 313 `shouldBe` '\x0252');
				it "IPA Number 169 to latin small letter turned w"
					(numberToUnicode 169 `shouldBe` 'ʍ');
				it "latin small letter turned w code point"
					(numberToUnicode 169 `shouldBe` '\x028D');
				it "IPA Number 170 to latin small letter w"
					(numberToUnicode 170 `shouldBe` 'w');
				it "latin small letter w code point"
					(numberToUnicode 170 `shouldBe` '\x0077');
				it "IPA Number 171 to latin small letter turned h"
					(numberToUnicode 171 `shouldBe` 'ɥ');
				it "latin small letter turned h code point"
					(numberToUnicode 171 `shouldBe` '\x0265');
				it "IPA Number 172 to latin letter small capital h"
					(numberToUnicode 172 `shouldBe` 'ʜ');
				it "latin letter small capital h code point"
					(numberToUnicode 172 `shouldBe` '\x029C');
				it "IPA Number 174 to latin letter reversed glottal stop with stroke"
					(numberToUnicode 174 `shouldBe` 'ʢ');
				it "latin letter reversed glottal stop with stroke code point"
					(numberToUnicode 174 `shouldBe` '\x02A2');
				it "IPA Number 173 to latin letter glottal stop with stroke"
					(numberToUnicode 173 `shouldBe` 'ʡ');
				it "latin letter glottal stop with stroke code point"
					(numberToUnicode 173 `shouldBe` '\x02A1');
				it "IPA Number 182 to latin small letter c with curl"
					(numberToUnicode 182 `shouldBe` 'ɕ');
				it "latin small letter c with curl code point"
					(numberToUnicode 182 `shouldBe` '\x0255');
				it "IPA Number 183 to latin small letter z with curl"
					(numberToUnicode 183 `shouldBe` 'ʑ');
				it "latin small letter z with curl code point"
					(numberToUnicode 183 `shouldBe` '\x0291');
				it "IPA Number 181 to latin small letter turned r with long leg"
					(numberToUnicode 181 `shouldBe` 'ɺ');
				it "latin small letter turned r with long leg code point"
					(numberToUnicode 181 `shouldBe` '\x027A');
				it "IPA Number 175 to latin small letter heng with hook"
					(numberToUnicode 175 `shouldBe` 'ɧ');
				it "latin small letter heng with hook code point"
					(numberToUnicode 175 `shouldBe` '\x0267');
				it "IPA Number 433 to combining double inverted breve"
					(numberToUnicode 433 `shouldBe` '͡');
				it "combining double inverted breve code point"
					(numberToUnicode 433 `shouldBe` '\x0361');
{-				it "IPA Number 433 to combining double breve below"
					(numberToUnicode 433 `shouldBe` '͜');
				it "combining double breve below code point"
					(numberToUnicode 433 `shouldBe` '\x035C');
-}
				it "IPA Number 501 to modifier letter vertical line"
					(numberToUnicode 501 `shouldBe` 'ˈ');
				it "modifier letter vertical line code point"
					(numberToUnicode 501 `shouldBe` '\x02C8');
				it "IPA Number 502 to modifier letter low vertical line"
					(numberToUnicode 502 `shouldBe` 'ˌ');
				it "modifier letter low vertical line code point"
					(numberToUnicode 502 `shouldBe` '\x02CC');
				it "IPA Number 503 to modifier letter triangular colon"
					(numberToUnicode 503 `shouldBe` 'ː');
				it "modifier letter triangular colon code point"
					(numberToUnicode 503 `shouldBe` '\x02D0');
				it "IPA Number 504 to modifier letter half triangular colon"
					(numberToUnicode 504 `shouldBe` 'ˑ');
				it "modifier letter half triangular colon code point"
					(numberToUnicode 504 `shouldBe` '\x02D1');
				it "IPA Number 505 to combining breve"
					(numberToUnicode 505 `shouldBe` '̆');
				it "combining breve code point"
					(numberToUnicode 505 `shouldBe` '\x0306');
				it "IPA Number 507 to vertical line"
					(numberToUnicode 507 `shouldBe` '|');
				it "vertical line code point"
					(numberToUnicode 507 `shouldBe` '\x007C');
				it "IPA Number 508 to double vertical line"
					(numberToUnicode 508 `shouldBe` '‖');
				it "double vertical line code point"
					(numberToUnicode 508 `shouldBe` '\x2016');
				it "IPA Number 506 to full stop"
					(numberToUnicode 506 `shouldBe` '.');
				it "full stop code point"
					(numberToUnicode 506 `shouldBe` '\x002E');
				it "IPA Number 509 to undertie"
					(numberToUnicode 509 `shouldBe` '‿');
				it "undertie code point"
					(numberToUnicode 509 `shouldBe` '\x203F');
				it "IPA Number 402 to combining ring below"
					(numberToUnicode 402 `shouldBe` '̥');
				it "combining ring below code point"
					(numberToUnicode 402 `shouldBe` '\x0325');


				it "IPA Number 403 to combining caron below"
					(numberToUnicode 403 `shouldBe` '̬');
				it "combining caron below code point"
					(numberToUnicode 403 `shouldBe` '\x032C');
				it "IPA Number 404 to modifier letter small h"
					(numberToUnicode 404 `shouldBe` 'ʰ');
				it "modifier letter small h code point"
					(numberToUnicode 404 `shouldBe` '\x02B0');
				it "IPA Number 411 to combining right half ring below"
					(numberToUnicode 411 `shouldBe` '̹');
				it "combining right half ring below code point"
					(numberToUnicode 411 `shouldBe` '\x0339');
				it "IPA Number 412 to combining left half ring below"
					(numberToUnicode 412 `shouldBe` '̜');
				it "combining left half ring below code point"
					(numberToUnicode 412 `shouldBe` '\x031C');
				it "IPA Number 413 to combining plus sign below"
					(numberToUnicode 413 `shouldBe` '̟');
				it "combining plus sign below"
					(numberToUnicode 413 `shouldBe` '\x031F');
				it "IPA Number 414 to combining minus sign below"
					(numberToUnicode 414 `shouldBe` '̠');
				it "combining minus sign below code point"
					(numberToUnicode 414 `shouldBe` '\x0320');
				it "IPA Number 415 to combining diaeresis"
					(numberToUnicode 415 `shouldBe` '̈');
				it "combining diaeresis code point"
					(numberToUnicode 415 `shouldBe` '\x0308');
				it "IPA Number 416 to combining x above"
					(numberToUnicode 416 `shouldBe` '̽');
				it "combining x above code point"
					(numberToUnicode 416 `shouldBe` '\x033D');

				it "IPA Number 431 to combining vertical line below"
					(numberToUnicode 431 `shouldBe` '̩');
				it "combining vertical line below code point"
					(numberToUnicode 431 `shouldBe` '\x0329');
				it "IPA Number 432 to combining inverted breve below"
					(numberToUnicode 432 `shouldBe` '̯');
				it "combining inverted breve below code point"
					(numberToUnicode 432 `shouldBe` '\x032F');
				it "IPA Number 419 to modifier letter rhotic hook"
					(numberToUnicode 419 `shouldBe` '˞');
				it "modifier letter rhotic hook code point"
					(numberToUnicode 419 `shouldBe` '\x02DE');
				it "IPA Number 405 to combining diaeresis below"
					(numberToUnicode 405 `shouldBe` '̤');
				it "combining diaeresis below code point"
					(numberToUnicode 405 `shouldBe` '\x0324');
				it "IPA Number 406 to combining tilde below"
					(numberToUnicode 406 `shouldBe` '̰');
				it "combining tilde below code point"
					(numberToUnicode 406 `shouldBe` '\x0330');
				it "IPA Number 407 to combining seagul below"
					(numberToUnicode 407 `shouldBe` '̼');
				it "combining seagul below code point"
					(numberToUnicode 407 `shouldBe` '\x033C');
				it "IPA Number 420 to modifier letter small w"
					(numberToUnicode 420 `shouldBe` 'ʷ');
				it "modifier letter small w code point"
					(numberToUnicode 420 `shouldBe` '\x02B7');
				it "IPA Number 421 to modifier letter small j"
					(numberToUnicode 421 `shouldBe` 'ʲ');
				it "modifier letter small j code point"
					(numberToUnicode 421 `shouldBe` '\x02B2');
				it "IPA Number 422 to modifier letter small gamma"
					(numberToUnicode 422 `shouldBe` 'ˠ');
				it "modifier letter small gamma code point"
					(numberToUnicode 422 `shouldBe` '\x02E0');
				it "IPA Number 423 to modifier letter small reversed glottal stop"
					(numberToUnicode 423 `shouldBe` 'ˤ');
				it "modifier letter small reversed glottal stop code point"
					(numberToUnicode 423 `shouldBe` '\x02E4');
				it "IPA Number 428 to combining tilde overlay"
					(numberToUnicode 428 `shouldBe` '̴');
				it "combining tilde overlay code point"
					(numberToUnicode 428 `shouldBe` '\x0334');
				it "IPA Number 429 to combining up tack below"
					(numberToUnicode 429 `shouldBe` '̝');
				it "combining up tack below code point"
					(numberToUnicode 429 `shouldBe` '\x031D');
				it "IPA Number 430 to combining down tack below"
					(numberToUnicode 430 `shouldBe` '̞');
				it "combining down tack below code point"
					(numberToUnicode 430 `shouldBe` '\x031E');
				it "IPA Number 417 to combining left tack below"
					(numberToUnicode 417 `shouldBe` '̘');
				it "combining left tack below code point"
					(numberToUnicode 417 `shouldBe` '\x0318');
				it "IPA Number 418 to combining right tack below"
					(numberToUnicode 418 `shouldBe` '̙');
				it "combining right tack below code point"
					(numberToUnicode 418 `shouldBe` '\x0319');
				it "IPA Number 408 to combining bridge below"
					(numberToUnicode 408 `shouldBe` '̪');
				it "combining bridge below code point"
					(numberToUnicode 408 `shouldBe` '\x032A');
				it "IPA Number 409 to combining inverted bridge below"
					(numberToUnicode 409 `shouldBe` '̺');
				it "combining inverted bridge below code point"
					(numberToUnicode 409 `shouldBe` '\x033A');
				it "IPA Number 410 to combining square below"
					(numberToUnicode 410 `shouldBe` '̻');
				it "combining square below code point"
					(numberToUnicode 410 `shouldBe` '\x033B');
				it "IPA Number 424 to combining tilde"
					(numberToUnicode 424 `shouldBe` '̃');
				it "combining tilde code point"
					(numberToUnicode 424 `shouldBe` '\x0303');
				it "IPA Number 425 to superscript latin small letter n"
					(numberToUnicode 425 `shouldBe` 'ⁿ');
				it "superscript latin small letter n code point"
					(numberToUnicode 425 `shouldBe` '\x207F');
				it "IPA Number 426 to modifier letter small l"
					(numberToUnicode 426 `shouldBe` 'ˡ');
				it "modifier letter small l code point"
					(numberToUnicode 426 `shouldBe` '\x02E1');
				it "IPA Number 427 to combining left angle above"
					(numberToUnicode 427 `shouldBe` '̚');
				it "combining left angle above code point"
					(numberToUnicode 427 `shouldBe` '\x031A');
				it "IPA Number 512 to combining double acute accent"
					(numberToUnicode 512 `shouldBe` '̋');
				it "combining double acute accent code point"
					(numberToUnicode 512 `shouldBe` '\x030B');
				it "IPA Number 513 to combining acute accent"
					(numberToUnicode 513 `shouldBe` '́');
				it "combining acute accent code point"
					(numberToUnicode 513 `shouldBe` '\x0301');
				it "IPA Number 514 to combining macron"
					(numberToUnicode 514 `shouldBe` '̄');
				it "combining macron code point"
					(numberToUnicode 514 `shouldBe` '\x0304');
				it "IPA Number 515 to combining grave accent"
					(numberToUnicode 515 `shouldBe` '̀');
				it "combining grave accent code point"
					(numberToUnicode 515 `shouldBe` '\x0300');
				it "IPA Number 516 to combining double grave accent"
					(numberToUnicode 516 `shouldBe` '̏');
				it "combining double grave accent code point"
					(numberToUnicode 516 `shouldBe` '\x030F');
				it "IPA Number 517 to downwards arrow"
					(numberToUnicode 517 `shouldBe` '↓');
				it "downwards arrow code point"
					(numberToUnicode 517 `shouldBe` '\x2193');
				it "IPA Number 524 to combining caron"
					(numberToUnicode 524 `shouldBe` '̌');
				it "combining caron code point"
					(numberToUnicode 524 `shouldBe` '\x030C');
				it "IPA Number 525 to combining circumflex accent"
					(numberToUnicode 525 `shouldBe` '̂');
				it "combining circumflex accent code point"
					(numberToUnicode 525 `shouldBe` '\x0302');
				it "IPA Number 526 to combining macron acute"
					(numberToUnicode 526 `shouldBe` '᷄');
				it "combining macron acute code point"
					(numberToUnicode 526 `shouldBe` '\x1DC4');
				it "IPA Number 527 to combining grave macron"
					(numberToUnicode 527 `shouldBe` '᷅');
				it "combining grave macron code point"
					(numberToUnicode 527 `shouldBe` '\x1DC5');
				it "IPA Number 528 to combining grave acute grave" 
					(numberToUnicode 528 `shouldBe` '᷈');
				it "combining grave acute grave code point" 
					(numberToUnicode 528 `shouldBe` '\x1DC8');
				it "IPA Number 510 to north east arrow"
					(numberToUnicode 510 `shouldBe` '↗');
				it "north east arrow code point"
					(numberToUnicode 510 `shouldBe` '\x2197');
				it "IPA Number 511 to south east arrow"
					(numberToUnicode 511 `shouldBe` '↘');
				it "south east arrow code point"
					(numberToUnicode 511 `shouldBe` '\x2198');
				it "IPA Number 517 to downwards arrow"
					(numberToUnicode 517 `shouldBe` '↓');
				it "downwards arrow code point"
					(numberToUnicode 517 `shouldBe` '\x2193');
				it "IPA Number 518 to upwards arrow"
					(numberToUnicode 518 `shouldBe` '↑');
				it "upwards arrow code point"
					(numberToUnicode 518 `shouldBe` '\x2191');
				it "IPA Number 519 to modifier letter extra high tone bar"
					(numberToUnicode 519 `shouldBe` '˥');
				it "modifier letter extra high tone bar code point"
					(numberToUnicode 519 `shouldBe` '\x02E5');
				it "IPA Number 520 to modifier letter high tone bar"
					(numberToUnicode 520 `shouldBe` '˦');
				it "modifier letter high tone bar code point"
					(numberToUnicode 520 `shouldBe` '\x02E6');
				it "IPA Number 521 to modifier letter mid tone bar"
					(numberToUnicode 521 `shouldBe` '˧');
				it "modifier letter mid tone bar code point"
					(numberToUnicode 521 `shouldBe` '\x02E7');
				it "IPA Number 522 to modifier letter low tone bar"
					(numberToUnicode 522 `shouldBe` '˨');
				it "modifier letter low tone bar code point"
					(numberToUnicode 522 `shouldBe` '\x02E8');
				it "IPA Number 523 to modifier letter extra low tone bar"
					(numberToUnicode 523 `shouldBe` '˩');
				it "modifier letter extra low tone bar"
					(numberToUnicode 523 `shouldBe` '\x02E9');
			}
		)