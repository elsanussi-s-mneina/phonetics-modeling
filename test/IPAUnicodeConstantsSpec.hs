module IPAUnicodeConstantsSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import IPAConstants.IPAUnicodeConstants


ipaUnicodeSpec :: Spec
ipaUnicodeSpec =
	describe "Unicode code points for IPA characters (and diacritics)"
		(
			do
			{
				it "latin small letter p"
					(latin_small_letter_p `shouldBe` 'p');
				it "latin small letter p code point"
					(latin_small_letter_p `shouldBe` '\x0070');
				it "latin small letter b"
					(latin_small_letter_b `shouldBe` 'b');
				it "latin small letter b code point"
					(latin_small_letter_b `shouldBe` '\x0062');
				it "latin small letter t"
					(latin_small_letter_t `shouldBe` 't');
				it "latin small letter t code point"
					(latin_small_letter_t `shouldBe` '\x0074');
				it "latin small letter d"
					(latin_small_letter_d `shouldBe` 'd');
				it "latin small letter d code point"
					(latin_small_letter_d `shouldBe` '\x0064');
				it "latin small letter t with retroflex hook"
					(latin_small_letter_t_with_retroflex_hook `shouldBe` 'ʈ');
				it "latin small letter t with retroflex hook code point"
					(latin_small_letter_t_with_retroflex_hook `shouldBe` '\x0288');
				it "latin small letter d with tail"
					(latin_small_letter_d_with_tail `shouldBe` 'ɖ');
				it "latin small letter d with tail code point"
					(latin_small_letter_d_with_tail `shouldBe` '\x0256');
				it "latin small letter c"
					(latin_small_letter_c `shouldBe` 'c');
				it "latin small letter c code point"
					(latin_small_letter_c `shouldBe` '\x0063');
				it "latin small dotless j with stroke"
					(latin_small_letter_dotless_j_with_stroke `shouldBe` 'ɟ');
				it "latin small dotless j with stroke code point"
					(latin_small_letter_dotless_j_with_stroke `shouldBe` '\x025F');
				it "latin small letter k"
					(latin_small_letter_k `shouldBe` 'k');
				it "latin small letter k code point"
					(latin_small_letter_k `shouldBe` '\x006B');
				it "latin small letter g"
					(latin_small_letter_g `shouldBe` 'g');
				it "latin small letter g code point"
					(latin_small_letter_g `shouldBe` '\x0067');
				it "latin small letter script g"
					(latin_small_letter_script_g `shouldBe` 'ɡ');
				it "latin small letter script g code point"
					(latin_small_letter_script_g `shouldBe` '\x0261');
				it "latin small letter q"
					(latin_small_letter_q `shouldBe` 'q');
				it "latin small letter q code point"
					(latin_small_letter_q `shouldBe` '\x0071');
				it "latin letter small capital g"
					(latin_letter_small_capital_g `shouldBe` 'ɢ');
				it "latin letter small capital g code point"
					(latin_letter_small_capital_g `shouldBe` '\x0262');
				it "latin letter glottal stop"
					(latin_letter_glottal_stop `shouldBe` 'ʔ');
				it "latin letter glottal stop code point"
					(latin_letter_glottal_stop `shouldBe` '\x0294');
				it "latin small letter m"
					(latin_small_letter_m `shouldBe` 'm');
				it "latin small letter m code point"
					(latin_small_letter_m `shouldBe` '\x006D');
				it "latin small letter m with hook"
					(latin_small_letter_m_with_hook `shouldBe` 'ɱ');
				it "latin small letter m with hook code point"
					(latin_small_letter_m_with_hook `shouldBe` '\x0271');
				it "latin small letter n"
					(latin_small_letter_n `shouldBe` 'n');
				it "latin small letter n code point"
					(latin_small_letter_n `shouldBe` '\x006E');
				it "latin small letter n with retroflex hook"
					(latin_small_letter_n_with_retroflex_hook `shouldBe` 'ɳ');
				it "latin small letter n with retroflex hook code point"
					(latin_small_letter_n_with_retroflex_hook `shouldBe` '\x0273');
				it "latin small letter n with left hook"
					(latin_small_letter_n_with_left_hook `shouldBe` 'ɲ');
				it "latin small letter n with left hook code point"
					(latin_small_letter_n_with_left_hook `shouldBe` '\x0272');
				it "latin small letter eng"
					(latin_small_letter_eng `shouldBe` 'ŋ');
				it "latin small letter eng code point"
					(latin_small_letter_eng `shouldBe` '\x014B');
				it "latin letter small capital n"
					(latin_letter_small_capital_n `shouldBe` 'ɴ');
				it "latin letter small capital n code point"
					(latin_letter_small_capital_n `shouldBe` '\x0274');
				it "latin letter small capital b"
					(latin_letter_small_capital_b `shouldBe` 'ʙ');
				it "latin letter small capital b code point"
					(latin_letter_small_capital_b `shouldBe` '\x0299');
				it "latin small letter r"
					(latin_small_letter_r `shouldBe` 'r');
				it "latin small letter r code point"
					(latin_small_letter_r `shouldBe` '\x0072');
				it "latin letter small capital r"
					(latin_letter_small_capital_r `shouldBe` 'ʀ');
				it "latin letter small capital r code point"
					(latin_letter_small_capital_r `shouldBe` '\x0280');
				it "latin small letter v with right hook"
					(latin_small_letter_v_with_right_hook `shouldBe` 'ⱱ');
				it "latin small letter v with right hook code point"
					(latin_small_letter_v_with_right_hook `shouldBe` '\x2C71');
				it "latin small letter r with fishhook"
					(latin_small_letter_r_with_fishhook `shouldBe` 'ɾ');
				it "latin small letter r with fishhook code point"
					(latin_small_letter_r_with_fishhook `shouldBe` '\x027E');
				it "latin small letter r with tail"
					(latin_small_letter_r_with_tail `shouldBe` 'ɽ');
				it "latin small letter r with tail code point"
					(latin_small_letter_r_with_tail `shouldBe` '\x027D');
				it "latin small letter phi"
					(latin_small_letter_phi `shouldBe` 'ɸ');
				it "latin small letter phi code point"
					(latin_small_letter_phi `shouldBe` '\x0278');
				it "greek small letter beta"
					(greek_small_letter_beta `shouldBe` 'β');
				it "latin small letter beta code point"
					(greek_small_letter_beta `shouldBe` '\x03B2');
				it "latin small letter f"
					(latin_small_letter_f `shouldBe` 'f');
				it "latin small letter f code point"
					(latin_small_letter_f `shouldBe` '\x0066');
				it "latin small letter v"
					(latin_small_letter_v `shouldBe` 'v');
				it "latin small letter v code point"
					(latin_small_letter_v `shouldBe` '\x0076');
				it "greek small letter theta"
					(greek_small_letter_theta `shouldBe` 'θ');
				it "greek small letter theta code point"
					(greek_small_letter_theta `shouldBe` '\x03B8');
				it "latin small letter eth"
					(latin_small_letter_eth `shouldBe` 'ð');
				it "latin small letter eth code point"
					(latin_small_letter_eth `shouldBe` '\x00F0');
				it "latin small letter s"
					(latin_small_letter_s `shouldBe` 's');
				it "latin small letter s code point"
					(latin_small_letter_s `shouldBe` '\x0073');
				it "latin small letter z"
					(latin_small_letter_z `shouldBe` 'z');
				it "latin small letter z code point"
					(latin_small_letter_z `shouldBe` '\x007A');
				it "latin small letter esh"
					(latin_small_letter_esh `shouldBe` 'ʃ');
				it "latin small letter esh code point"
					(latin_small_letter_esh `shouldBe` '\x0283');
				it "latin small letter ezh"
					(latin_small_letter_ezh `shouldBe` 'ʒ');
				it "latin small letter ezh code point"
					(latin_small_letter_ezh `shouldBe` '\x0292');
				it "latin small letter s with hook"
					(latin_small_letter_s_with_hook `shouldBe` 'ʂ');
				it "latin small letter s with hook code point"
					(latin_small_letter_s_with_hook `shouldBe` '\x0282');
				it "latin small letter z with retroflex hook"
					(latin_small_letter_z_with_retroflex_hook `shouldBe` 'ʐ');
				it "latin small letter z with retroflex hook code point"
					(latin_small_letter_z_with_retroflex_hook `shouldBe` '\x0290');
				it "latin small letter c with cedilla"
					(latin_small_letter_c_with_cedilla `shouldBe` 'ç');
				it "latin small letter c with cedilla code point"
					(latin_small_letter_c_with_cedilla `shouldBe` '\x00E7');
				it "latin small letter j with crossed tail"
					(latin_small_letter_j_with_crossed_tail `shouldBe` 'ʝ');
				it "latin small letter j with crossed tail code point"
					(latin_small_letter_j_with_crossed_tail `shouldBe` '\x029D');
				it "latin small letter x"
					(latin_small_letter_x `shouldBe` 'x');
				it "latin small letter x code point"
					(latin_small_letter_x `shouldBe` '\x0078');
				it "latin small letter gamma"
					(latin_small_letter_gamma `shouldBe` 'ɣ');
				it "latin small letter gamma code point"
					(latin_small_letter_gamma `shouldBe` '\x0263');
				it "greek small letter chi"
					(greek_small_letter_chi `shouldBe` 'χ');
				it "greek small letter chi code point"
					(greek_small_letter_chi `shouldBe` '\x03C7');
				it "latin letter small capital inverted r"
					(latin_letter_small_capital_inverted_r `shouldBe` 'ʁ');
				it "latin letter small capital inverted r code point"
					(latin_letter_small_capital_inverted_r `shouldBe` '\x0281');
				it "latin small letter h with stroke"
					(latin_small_letter_h_with_stroke `shouldBe` 'ħ');
				it "latin small letter h with stroke code point"
					(latin_small_letter_h_with_stroke `shouldBe` '\x0127');
				it "latin letter pharyngeal voiced fricative"
					(latin_letter_pharyngeal_voiced_fricative `shouldBe` 'ʕ');
				it "latin letter pharyngeal voiced fricative code point"
					(latin_letter_pharyngeal_voiced_fricative `shouldBe` '\x0295');
				it "latin small letter h"
					(latin_small_letter_h `shouldBe` 'h');
				it "latin small letter h code point"
					(latin_small_letter_h `shouldBe` '\x0068');
				it "latin small letter h with hook"
					(latin_small_letter_h_with_hook `shouldBe` 'ɦ');
				it "latin small letter h with hook code point"
					(latin_small_letter_h_with_hook `shouldBe` '\x0266');
				it "latin small letter l with belt"
					(latin_small_letter_l_with_belt `shouldBe` 'ɬ');
				it "latin small letter l with belt code point"
					(latin_small_letter_l_with_belt `shouldBe` '\x026C');
				it "latin small letter lezh"
					(latin_small_letter_lezh `shouldBe` 'ɮ');
				it "latin small letter lezh code point"
					(latin_small_letter_lezh `shouldBe` '\x026E');
				it "latin small letter v with hook"
					(latin_small_letter_v_with_hook `shouldBe` 'ʋ');
				it "latin small letter v with hook code point"
					(latin_small_letter_v_with_hook `shouldBe` '\x028B');
				it "latin small letter turned r"
					(latin_small_letter_turned_r `shouldBe` 'ɹ');
				it "latin small letter turned r code point"
					(latin_small_letter_turned_r `shouldBe` '\x0279');
				it "latin small letter turned r with hook"
					(latin_small_letter_turned_r_with_hook `shouldBe` 'ɻ');
				it "latin small letter turned r with hook code point"
					(latin_small_letter_turned_r_with_hook `shouldBe` '\x027B');
				it "latin small letter j"
					(latin_small_letter_j `shouldBe` 'j');
				it "latin small letter j code point"
					(latin_small_letter_j `shouldBe` '\x006A');
				it "latin small letter turned m with long leg"
					(latin_small_letter_turned_m_with_long_leg `shouldBe` 'ɰ');
				it "latin small letter turned m with long leg code point"
					(latin_small_letter_turned_m_with_long_leg `shouldBe` '\x0270');
				it "latin small letter l"
					(latin_small_letter_l `shouldBe` 'l');
				it "latin small letter l code point"
					(latin_small_letter_l `shouldBe` '\x006C');
				it "latin small letter l with retroflex hook"
					(latin_small_letter_l_with_retroflex_hook `shouldBe` 'ɭ');
				it "latin small letter l with retroflex hook code point"
					(latin_small_letter_l_with_retroflex_hook `shouldBe` '\x026D');
				it "latin small letter turned y"
					(latin_small_letter_turned_y `shouldBe` 'ʎ');
				it "latin small letter turned y code point"
					(latin_small_letter_turned_y `shouldBe` '\x028E');
				it "latin letter small capital l"
					(latin_letter_small_capital_l `shouldBe` 'ʟ');
				it "latin letter small capital l code point"
					(latin_letter_small_capital_l `shouldBe` '\x029F');
				it "latin letter bilabial click"
					(latin_letter_bilabial_click `shouldBe` 'ʘ');
				it "latin letter bilabial click code point"
					(latin_letter_bilabial_click `shouldBe` '\x0298');
				it "latin letter dental click"
					(latin_letter_dental_click `shouldBe` 'ǀ');
				it "latin letter dental click code point"
					(latin_letter_dental_click `shouldBe` '\x01C0');
				it "latin letter retroflex click"
					(latin_letter_retroflex_click `shouldBe` 'ǃ');
				it "latin letter retroflex click"
					(latin_letter_retroflex_click `shouldBe` '\x01C3');
				it "latin letter alveolar click"
					(latin_letter_alveolar_click `shouldBe` 'ǂ');
				it "latin letter alveolar click code point"
					(latin_letter_alveolar_click `shouldBe` '\x01C2');
				it "latin letter lateral click"
					(latin_letter_lateral_click `shouldBe` 'ǁ');
				it "latin letter lateral click code point"
					(latin_letter_lateral_click `shouldBe` '\x01C1');
				it "latin small letter b with hook"
					(latin_small_letter_b_with_hook `shouldBe` 'ɓ');
				it "latin small letter b with hook code point"
					(latin_small_letter_b_with_hook `shouldBe` '\x0253');
				it "latin small letter d with hook"
					(latin_small_letter_d_with_hook `shouldBe` 'ɗ');
				it "latin small letter d with hook code point"
					(latin_small_letter_d_with_hook `shouldBe` '\x0257');
				it "latin small letter dotless j with stroke and hook"
					(latin_small_letter_dotless_j_with_stroke_and_hook `shouldBe` 'ʄ');
				it "latin small letter dotless j with stroke and hook code point"
					(latin_small_letter_dotless_j_with_stroke_and_hook `shouldBe` '\x0284');
				it "latin small letter g with hook"
					(latin_small_letter_g_with_hook `shouldBe` 'ɠ');
				it "latin small letter g with hook code point"
					(latin_small_letter_g_with_hook `shouldBe` '\x0260');
				it "latin letter small capital g with hook"
					(latin_letter_small_capital_g_with_hook `shouldBe` 'ʛ');
				it "latin letter small capital g with hook code point"
					(latin_letter_small_capital_g_with_hook `shouldBe` '\x029B');
				it "modifier letter apostrophe"
					(modifier_letter_apostrophe `shouldBe` 'ʼ');
				it "modifier letter apostrophe code point"
					(modifier_letter_apostrophe `shouldBe` '\x02BC');
				it "latin small letter i"
					(latin_small_letter_i `shouldBe` 'i');
				it "latin small letter i code point"
					(latin_small_letter_i `shouldBe` '\x0069');
				it "latin small letter y"
					(latin_small_letter_y `shouldBe` 'y');
				it "latin small letter y code point"
					(latin_small_letter_y `shouldBe` '\x0079');
				it "latin small letter i with stroke"
					(latin_small_letter_i_with_stroke `shouldBe` 'ɨ');
				it "latin small letter i with stroke code point"
					(latin_small_letter_i_with_stroke `shouldBe` '\x0268');
				it "latin small letter u bar"
					(latin_small_letter_u_bar `shouldBe` 'ʉ');
				it "latin small letter u bar code point"
					(latin_small_letter_u_bar `shouldBe` '\x0289');
				it "latin small letter turned m"
					(latin_small_letter_turned_m `shouldBe` 'ɯ');
				it "latin small letter turned m code point"
					(latin_small_letter_turned_m `shouldBe` '\x026F');
				it "latin small letter u"
					(latin_small_letter_u `shouldBe` 'u');
				it "latin small letter u code point"
					(latin_small_letter_u `shouldBe` '\x0075');
				it "latin letter small capital i"
					(latin_letter_small_capital_i `shouldBe` 'ɪ');
				it "latin letter small capital i code paint"
					(latin_letter_small_capital_i `shouldBe` '\x026A');
				it "latin letter small capital y"
					(latin_letter_small_capital_y `shouldBe` 'ʏ');
				it "latin letter small capital y code point"
					(latin_letter_small_capital_y `shouldBe` '\x028F');
				it "latin small letter upsilon"
					(latin_small_letter_upsilon `shouldBe` 'ʊ');
				it "latin small letter upsilon code point"
					(latin_small_letter_upsilon `shouldBe` '\x028A');
				it "latin small letter e"
					(latin_small_letter_e `shouldBe` 'e');
				it "latin small letter e code point"
					(latin_small_letter_e `shouldBe` '\x0065');
				it "latin small letter o with stroke"
					(latin_small_letter_o_with_stroke `shouldBe` 'ø');
				it "latin small letter o with stroke code point"
					(latin_small_letter_o_with_stroke `shouldBe` '\x00F8');
				it "latin small letter reversed e"
					(latin_small_letter_reversed_e `shouldBe` 'ɘ');
				it "latin small letter reversed e code point"
					(latin_small_letter_reversed_e `shouldBe` '\x0258');
				it "latin small letter barred o"
					(latin_small_letter_barred_o `shouldBe` 'ɵ');
				it "latin small letter barred o code point"
					(latin_small_letter_barred_o `shouldBe` '\x0275');
				it "latin small letter rams horn"
					(latin_small_letter_rams_horn `shouldBe` 'ɤ');
				it "latin small letter rams horn code point"
					(latin_small_letter_rams_horn `shouldBe` '\x0264');
				it "latin small letter o"
					(latin_small_letter_o `shouldBe` 'o');
				it "latin small letter o code point"
					(latin_small_letter_o `shouldBe` '\x006F');
				it "latin small letter schwa"
					(latin_small_letter_schwa `shouldBe` 'ə');
				it "latin small letter schwa code point"
					(latin_small_letter_schwa `shouldBe` '\x0259');
				it "latin small letter open e"
					(latin_small_letter_open_e `shouldBe` 'ɛ');
				it "latin small letter open e code point"
					(latin_small_letter_open_e `shouldBe` '\x025B');
				it "latin small ligature oe"
					(latin_small_ligature_oe `shouldBe` 'œ');
				it "latin small ligature oe code point"
					(latin_small_ligature_oe `shouldBe` '\x0153');
				it "latin small letter reversed open e"
					(latin_small_letter_reversed_open_e `shouldBe` 'ɜ');
				it "latin small letter reversed open e code point"
					(latin_small_letter_reversed_open_e `shouldBe` '\x025C');
				it "latin small letter closed reversed open e"
					(latin_small_letter_closed_reversed_open_e `shouldBe` 'ɞ');
				it "latin small letter closed reversed open e code point"
					(latin_small_letter_closed_reversed_open_e `shouldBe` '\x025E');
				it "latin small letter turned v"
					(latin_small_letter_turned_v `shouldBe` 'ʌ');
				it "latin small letter turned v code point"
					(latin_small_letter_turned_v `shouldBe` '\x028C');
				it "latin small letter open o"
					(latin_small_letter_open_o `shouldBe` 'ɔ');
				it "latin small letter open o code point"
					(latin_small_letter_open_o `shouldBe` '\x0254');
				it "latin small letter ae"
					(latin_small_letter_ae `shouldBe` 'æ');
				it "latin small letter ae code point"
					(latin_small_letter_ae `shouldBe` '\x00E6');
				it "latin small letter turned a"
					(latin_small_letter_turned_a `shouldBe` 'ɐ');
				it "latin small letter turned a code point"
					(latin_small_letter_turned_a `shouldBe` '\x0250');
				it "latin small letter a"
					(latin_small_letter_a `shouldBe` 'a');
				it "latin small letter a code point"
					(latin_small_letter_a `shouldBe` '\x0061');
				it "latin letter small capital oe"
					(latin_letter_small_capital_oe `shouldBe` 'ɶ');
				it "latin letter small capital oe code point"
					(latin_letter_small_capital_oe `shouldBe` '\x0276');
				it "latin small letter alpha"
					(latin_small_letter_alpha `shouldBe` 'ɑ');
				it "latin small letter alpha code point"
					(latin_small_letter_alpha `shouldBe` '\x0251');
				it "latin small letter turned alpha"
					(latin_small_letter_turned_alpha `shouldBe` 'ɒ');
				it "latin small letter turned alpha code point"
					(latin_small_letter_turned_alpha `shouldBe` '\x0252');
				it "latin small letter turned w"
					(latin_small_letter_turned_w `shouldBe` 'ʍ');
				it "latin small letter turned w code point"
					(latin_small_letter_turned_w `shouldBe` '\x028D');
				it "latin small letter w"
					(latin_small_letter_w `shouldBe` 'w');
				it "latin small letter w code point"
					(latin_small_letter_w `shouldBe` '\x0077');
				it "latin small letter turned h"
					(latin_small_letter_turned_h `shouldBe` 'ɥ');
				it "latin small letter turned h code point"
					(latin_small_letter_turned_h `shouldBe` '\x0265');
				it "latin letter small capital h"
					(latin_letter_small_capital_h `shouldBe` 'ʜ');
				it "latin letter small capital h code point"
					(latin_letter_small_capital_h `shouldBe` '\x029C');
				it "latin letter reversed glottal stop with stroke"
					(latin_letter_reversed_glottal_stop_with_stroke `shouldBe` 'ʢ');
				it "latin letter reversed glottal stop with stroke code point"
					(latin_letter_reversed_glottal_stop_with_stroke `shouldBe` '\x02A2');
				it "latin letter glottal stop with stroke"
					(latin_letter_glottal_stop_with_stroke `shouldBe` 'ʡ');
				it "latin letter glottal stop with stroke code point"
					(latin_letter_glottal_stop_with_stroke `shouldBe` '\x02A1');
				it "latin small letter c with curl"
					(latin_small_letter_c_with_curl `shouldBe` 'ɕ');
				it "latin small letter c with curl code point"
					(latin_small_letter_c_with_curl `shouldBe` '\x0255');
				it "latin small letter z with curl"
					(latin_small_letter_z_with_curl `shouldBe` 'ʑ');
				it "latin small letter z with curl code point"
					(latin_small_letter_z_with_curl `shouldBe` '\x0291');
				it "latin small letter turned r with long leg"
					(latin_small_letter_turned_r_with_long_leg `shouldBe` 'ɺ');
				it "latin small letter turned r with long leg code point"
					(latin_small_letter_turned_r_with_long_leg `shouldBe` '\x027A');
				it "latin small letter heng with hook"
					(latin_small_letter_heng_with_hook `shouldBe` 'ɧ');
				it "latin small letter heng with hook code point"
					(latin_small_letter_heng_with_hook `shouldBe` '\x0267');
				it "combining double inverted breve"
					(combining_double_inverted_breve `shouldBe` '͡');
				it "combining double inverted breve code point"
					(combining_double_inverted_breve `shouldBe` '\x0361');
				it "combining double breve below"
					(combining_double_breve_below `shouldBe` '͜');
				it "combining double breve below code point"
					(combining_double_breve_below `shouldBe` '\x035C');
				it "modifier letter vertical line"
					(modifier_letter_vertical_line `shouldBe` 'ˈ');
				it "modifier letter vertical line code point"
					(modifier_letter_vertical_line `shouldBe` '\x02C8');
				it "modifier letter low vertical line"
					(modifier_letter_low_vertical_line `shouldBe` 'ˌ');
				it "modifier letter low vertical line code point"
					(modifier_letter_low_vertical_line `shouldBe` '\x02CC');
				it "modifier letter triangular colon"
					(modifier_letter_triangular_colon `shouldBe` 'ː');
				it "modifier letter triangular colon code point"
					(modifier_letter_triangular_colon `shouldBe` '\x02D0');
				it "modifier letter half triangular colon"
					(modifier_letter_half_triangular_colon `shouldBe` 'ˑ');
				it "modifier letter half triangular colon code point"
					(modifier_letter_half_triangular_colon `shouldBe` '\x02D1');
				it "combining breve"
					(combining_breve `shouldBe` '̆');
				it "combining breve code point"
					(combining_breve `shouldBe` '\x0306');
				it "vertical line"
					(vertical_line `shouldBe` '|');
				it "vertical line code point"
					(vertical_line `shouldBe` '\x007C');
				it "double vertical line"
					(double_vertical_line `shouldBe` '‖');
				it "double vertical line code point"
					(double_vertical_line `shouldBe` '\x2016');
				it "full stop"
					(full_stop `shouldBe` '.');
				it "full stop code point"
					(full_stop `shouldBe` '\x002E');
				it "undertie"
					(undertie `shouldBe` '‿');
				it "undertie code point"
					(undertie `shouldBe` '\x203F');
				it "combining ring below"
					(combining_ring_below `shouldBe` '̥');
				it "combining ring below code point"
					(combining_ring_below `shouldBe` '\x0325');
				it "combining ring above"
					(combining_ring_above `shouldBe` '̊');
				it "combining ring above code point"
					(combining_ring_above `shouldBe` '\x030A');


				it "combining caron below"
					(combining_caron_below `shouldBe` '̬');
				it "combining caron below code point"
					(combining_caron_below `shouldBe` '\x032C');
				it "modifier letter small h"
					(modifier_letter_small_h `shouldBe` 'ʰ');
				it "modifier letter small h code point"
					(modifier_letter_small_h `shouldBe` '\x02B0');
				it "combining right half ring below"
					(combining_right_half_ring_below `shouldBe` '̹');
				it "combining right half ring below code point"
					(combining_right_half_ring_below `shouldBe` '\x0339');
				it "combining left half ring below"
					(combining_left_half_ring_below `shouldBe` '̜');
				it "combining left half ring below code point"
					(combining_left_half_ring_below `shouldBe` '\x031C');
				it "combining plus sign below"
					(combining_plus_sign_below `shouldBe` '̟');
				it "combining plus sign below"
					(combining_plus_sign_below `shouldBe` '\x031F');
				it "combining minus sign below"
					(combining_minus_sign_below `shouldBe` '̠');
				it "combining minus sign below code point"
					(combining_minus_sign_below `shouldBe` '\x0320');
				it "combining diaeresis"
					(combining_diaeresis `shouldBe` '̈');
				it "combining diaeresis code point"
					(combining_diaeresis `shouldBe` '\x0308');
				it "combining x above"
					(combining_x_above `shouldBe` '̽');
				it "combining x above code point"
					(combining_x_above `shouldBe` '\x033D');

				it "combining vertical line above"
					(combining_vertical_line_above `shouldBe` '̍');
				it "combining vertical line above code point"
					(combining_vertical_line_above `shouldBe` '\x030D');
				it "combining vertical line below"
					(combining_vertical_line_below `shouldBe` '̩');
				it "combining vertical line below code point"
					(combining_vertical_line_below `shouldBe` '\x0329');
				it "combining dot above"
					(combining_dot_above `shouldBe` '̇');
				it "combining dot above code point"
					(combining_dot_above `shouldBe` '\x0307');
				it "combining dot below"
					(combining_dot_below `shouldBe` '̣');
				it "combining dot below code point"
					(combining_dot_below `shouldBe` '\x0323');
				it "combining inverted breve below"
					(combining_inverted_breve_below `shouldBe` '̯');
				it "combining inverted breve below code point"
					(combining_inverted_breve_below `shouldBe` '\x032F');
				it "modifier letter rhotic hook"
					(modifier_letter_rhotic_hook `shouldBe` '˞');
				it "modifier letter rhotic hook code point"
					(modifier_letter_rhotic_hook `shouldBe` '\x02DE');
				it "combining diaeresis below"
					(combining_diaeresis_below `shouldBe` '̤');
				it "combining diaeresis below code point"
					(combining_diaeresis_below `shouldBe` '\x0324');
				it "combining tilde below"
					(combining_tilde_below `shouldBe` '̰');
				it "combining tilde below code point"
					(combining_tilde_below `shouldBe` '\x0330');
				it "combining seagul below"
					(combining_seagul_below `shouldBe` '̼');
				it "combining seagul below code point"
					(combining_seagul_below `shouldBe` '\x033C');
				it "modifier letter small w"
					(modifier_letter_small_w `shouldBe` 'ʷ');
				it "modifier letter small w code point"
					(modifier_letter_small_w `shouldBe` '\x02B7');
				it "modifier letter small j"
					(modifier_letter_small_j `shouldBe` 'ʲ');
				it "modifier letter small j code point"
					(modifier_letter_small_j `shouldBe` '\x02B2');
				it "modifier letter small gamma"
					(modifier_letter_small_gamma `shouldBe` 'ˠ');
				it "modifier letter small gamma code point"
					(modifier_letter_small_gamma `shouldBe` '\x02E0');
				it "modifier letter small reversed glottal stop"
					(modifier_letter_small_reversed_glottal_stop `shouldBe` 'ˤ');
				it "modifier letter small reversed glottal stop code point"
					(modifier_letter_small_reversed_glottal_stop `shouldBe` '\x02E4');
				it "combining tilde overlay"
					(combining_tilde_overlay `shouldBe` '̴');
				it "combining tilde overlay code point"
					(combining_tilde_overlay `shouldBe` '\x0334');
				it "combining up tack below"
					(combining_up_tack_below `shouldBe` '̝');
				it "combining up tack below code point"
					(combining_up_tack_below `shouldBe` '\x031D');
				it "combining down tack below"
					(combining_down_tack_below `shouldBe` '̞');
				it "combining down tack below code point"
					(combining_down_tack_below `shouldBe` '\x031E');
				it "combining left tack below"
					(combining_left_tack_below `shouldBe` '̘');
				it "combining left tack below code point"
					(combining_left_tack_below `shouldBe` '\x0318');
				it "combining right tack below"
					(combining_right_tack_below `shouldBe` '̙');
				it "combining right tack below code point"
					(combining_right_tack_below `shouldBe` '\x0319');
				it "combining bridge below"
					(combining_bridge_below `shouldBe` '̪');
				it "combining bridge below code point"
					(combining_bridge_below `shouldBe` '\x032A');
				it "combining inverted bridge below"
					(combining_inverted_bridge_below `shouldBe` '̺');
				it "combining inverted bridge below code point"
					(combining_inverted_bridge_below `shouldBe` '\x033A');
				it "combining square below"
					(combining_square_below `shouldBe` '̻');
				it "combining square below code point"
					(combining_square_below `shouldBe` '\x033B');
				it "combining tilde"
					(combining_tilde `shouldBe` '̃');
				it "combining tilde code point"
					(combining_tilde `shouldBe` '\x0303');
				it "superscript latin small letter n"
					(superscript_latin_small_letter_n `shouldBe` 'ⁿ');
				it "superscript latin small letter n code point"
					(superscript_latin_small_letter_n `shouldBe` '\x207F');
				it "modifier letter small l"
					(modifier_letter_small_l `shouldBe` 'ˡ');
				it "modifier letter small l code point"
					(modifier_letter_small_l `shouldBe` '\x02E1');
				it "combining left angle above"
					(combining_left_angle_above `shouldBe` '̚');
				it "combining left angle above code point"
					(combining_left_angle_above `shouldBe` '\x031A');
				it "combining double acute accent"
					(combining_double_acute_accent `shouldBe` '̋');
				it "combining double acute accent code point"
					(combining_double_acute_accent `shouldBe` '\x030B');
				it "combining acute accent"
					(combining_acute_accent `shouldBe` '́');
				it "combining acute accent code point"
					(combining_acute_accent `shouldBe` '\x0301');
				it "combining macron"
					(combining_macron `shouldBe` '̄');
				it "combining macron code point"
					(combining_macron `shouldBe` '\x0304');
				it "combining grave accent"
					(combining_grave_accent `shouldBe` '̀');
				it "combining grave accent code point"
					(combining_grave_accent `shouldBe` '\x0300');
				it "combining double grave accent"
					(combining_double_grave_accent `shouldBe` '̏');
				it "combining double grave accent code point"
					(combining_double_grave_accent `shouldBe` '\x030F');
				it "downwards arrow"
					(downwards_arrow `shouldBe` '↓');
				it "downwards arrow code point"
					(downwards_arrow `shouldBe` '\x2193');
				it "combining caron"
					(combining_caron `shouldBe` '̌');
				it "combining caron code point"
					(combining_caron `shouldBe` '\x030C');
				it "combining circumflex accent"
					(combining_circumflex_accent `shouldBe` '̂');
				it "combining circumflex accent code point"
					(combining_circumflex_accent `shouldBe` '\x0302');
				it "combining macron acute"
					(combining_macron_acute `shouldBe` '᷄');
				it "combining macron acute code point"
					(combining_macron_acute `shouldBe` '\x1DC4');
				it "combining grave macron"
					(combining_grave_macron `shouldBe` '᷅');
				it "combining grave macron code point"
					(combining_grave_macron `shouldBe` '\x1DC5');
				it "combining grave acute grave" 
					(combining_grave_acute_grave `shouldBe` '᷈');
				it "combining grave acute grave code point" 
					(combining_grave_acute_grave `shouldBe` '\x1DC8');
				it "north east arrow"
					(north_east_arrow `shouldBe` '↗');
				it "north east arrow code point"
					(north_east_arrow `shouldBe` '\x2197');
				it "south east arrow"
					(south_east_arrow `shouldBe` '↘');
				it "south east arrow code point"
					(south_east_arrow `shouldBe` '\x2198');
				it "downwards arrow"
					(downwards_arrow `shouldBe` '↓');
				it "downwards arrow code point"
					(downwards_arrow `shouldBe` '\x2193');
				it "upwards arrow"
					(upwards_arrow `shouldBe` '↑');
				it "upwards arrow code point"
					(upwards_arrow `shouldBe` '\x2191');
				it "modifier letter extra high tone bar"
					(modifier_letter_extra_high_tone_bar `shouldBe` '˥');
				it "modifier letter extra high tone bar code point"
					(modifier_letter_extra_high_tone_bar `shouldBe` '\x02E5');
				it "modifier letter high tone bar"
					(modifier_letter_high_tone_bar `shouldBe` '˦');
				it "modifier letter high tone bar code point"
					(modifier_letter_high_tone_bar `shouldBe` '\x02E6');
				it "modifier letter mid tone bar"
					(modifier_letter_mid_tone_bar `shouldBe` '˧');
				it "modifier letter mid tone bar code point"
					(modifier_letter_mid_tone_bar `shouldBe` '\x02E7');
				it "modifier letter low tone bar"
					(modifier_letter_low_tone_bar `shouldBe` '˨');
				it "modifier letter low tone bar code point"
					(modifier_letter_low_tone_bar `shouldBe` '\x02E8');
				it "modifier letter extra low tone bar"
					(modifier_letter_extra_low_tone_bar `shouldBe` '˩');
				it "modifier letter extra low tone bar"
					(modifier_letter_extra_low_tone_bar `shouldBe` '\x02E9');
			}
		)