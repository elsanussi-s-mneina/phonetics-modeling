module IPANumberConstantsSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import IPAConstants.IPANumbers

-- See The International Phonetic Alphabet Number Chart at https://www.internationalphoneticassociation.org/sites/default/files/IPA_Number_chart_(C)2005.pdf

ipaNumbersSpec :: Spec
ipaNumbersSpec =
	describe "IPA Numbers for IPA characters (and diacritics)"
		(
			do
			{
				it "latin small letter p should be IPA Number 101"
					(latin_small_letter_p `shouldBe` 101);
				it "latin small letter b should be IPA Number 102"
					(latin_small_letter_b `shouldBe` 102);
				it "latin small letter t should be IPA Number 103"
					(latin_small_letter_t `shouldBe` 103);
				it "latin small letter d should be IPA Number 104"
					(latin_small_letter_d `shouldBe` 104);
				it "latin small letter t with retroflex hook should be IPA Number 105"
					(latin_small_letter_t_with_retroflex_hook `shouldBe` 105);
				it "latin small letter d with tail should be IPA Number 106"
					(latin_small_letter_d_with_tail `shouldBe` 106);
				it "latin small letter c should be IPA Number 107"
					(latin_small_letter_c `shouldBe` 107);
				it "latin small dotless j with stroke should be IPA Number 108"
					(latin_small_letter_dotless_j_with_stroke `shouldBe` 108);
				it "latin small letter k should be IPA Number 109"
					(latin_small_letter_k `shouldBe` 109);
				it "latin small letter g should be IPA Number 110"
					(latin_small_letter_g `shouldBe` 110);
				it "latin small letter q should be IPA Number 111"
					(latin_small_letter_q `shouldBe` 111);
				it "latin letter small capital g should be IPA Number 112"
					(latin_letter_small_capital_g `shouldBe` 112);
				it "latin letter glottal stop should be IPA Number 113"
					(latin_letter_glottal_stop `shouldBe` 113);
				it "latin small letter m should be IPA Number 114"
					(latin_small_letter_m `shouldBe` 114);
				it "latin small letter m with hook should be IPA Number 115"
					(latin_small_letter_m_with_hook `shouldBe` 115);
				it "latin small letter n should be IPA Number 116"
					(latin_small_letter_n `shouldBe` 116);
				it "latin small letter n with retroflex hook should be IPA Number 117"
					(latin_small_letter_n_with_retroflex_hook `shouldBe` 117);
				it "latin small letter n with left hook should be IPA Number 118"
					(latin_small_letter_n_with_left_hook `shouldBe` 118);

				it "latin small letter eng should be IPA Number 119"
					(latin_small_letter_eng `shouldBe` 119);
				it "latin letter small capital n should be IPA Number 120"
					(latin_letter_small_capital_n `shouldBe` 120);
				it "latin letter small capital b should be IPA Number 121"
					(latin_letter_small_capital_b `shouldBe` 121);
				it "latin small letter r should be IPA Number 122"
					(latin_small_letter_r `shouldBe` 122);
				it "latin letter small capital r should be IPA Number 123"
					(latin_letter_small_capital_r `shouldBe` 123);
				it "latin small letter v with right hook should be IPA Number 184"
					(latin_small_letter_v_with_right_hook `shouldBe` 184);
				it "latin small letter r with fishhook should be IPA Number 124"
					(latin_small_letter_r_with_fishhook `shouldBe` 124);
				it "latin small letter r with tail should be IPA Number 125"
					(latin_small_letter_r_with_tail `shouldBe` 125);
				it "latin small letter phi should be IPA Number 126"
					(latin_small_letter_phi `shouldBe` 126);
				it "greek small letter beta should be IPA Number 127"
					(greek_small_letter_beta `shouldBe` 127);
				it "latin small letter f should be IPA Number 128"
					(latin_small_letter_f `shouldBe` 128);
				it "latin small letter v should be IPA Number 129"
					(latin_small_letter_v `shouldBe` 129);
				it "greek small letter theta should be IPA Number 130"
					(greek_small_letter_theta `shouldBe` 130);
				it "latin small letter eth should be IPA Number 131"
					(latin_small_letter_eth `shouldBe` 131);
				it "latin small letter s should be IPA Number 132"
					(latin_small_letter_s `shouldBe` 132);
				it "latin small letter z should be IPA Number 133"
					(latin_small_letter_z `shouldBe` 133);
				it "latin small letter esh should be IPA Number 134"
					(latin_small_letter_esh `shouldBe` 134);
				it "latin small letter ezh should be IPA Number 135"
					(latin_small_letter_ezh `shouldBe` 135);
				it "latin small letter s with hook should be IPA Number 136"
					(latin_small_letter_s_with_hook `shouldBe` 136);
				it "latin small letter z with retroflex hook should be IPA Number 137"
					(latin_small_letter_z_with_retroflex_hook `shouldBe` 137);

				it "latin small letter c with cedilla should be IPA Number 138"
					(latin_small_letter_c_with_cedilla `shouldBe` 138);
				it "latin small letter j with crossed tail should be IPA Number 139"
					(latin_small_letter_j_with_crossed_tail `shouldBe` 139);
				it "latin small letter x should be IPA Number 140"
					(latin_small_letter_x `shouldBe` 140);
				it "latin small letter gamma should be IPA Number 141"
					(latin_small_letter_gamma `shouldBe` 141);
				it "greek small letter chi should be IPA Number 142"
					(greek_small_letter_chi `shouldBe` 142);
				it "latin letter small capital inverted r should be IPA Number 143"
					(latin_letter_small_capital_inverted_r `shouldBe` 143);
				it "latin small letter h with stroke should be IPA Number 144"
					(latin_small_letter_h_with_stroke `shouldBe` 144);
				it "latin letter pharyngeal voiced fricative should be IPA Number 145"
					(latin_letter_pharyngeal_voiced_fricative `shouldBe` 145);
				it "latin small letter h should be IPA Number 146"
					(latin_small_letter_h `shouldBe` 146);
				it "latin small letter h with hook should be IPA Number 147"
					(latin_small_letter_h_with_hook `shouldBe` 147);
				it "latin small letter l with belt should be IPA Number 148"
					(latin_small_letter_l_with_belt `shouldBe` 148);
				it "latin small letter lezh should be IPA Number 149"
					(latin_small_letter_lezh `shouldBe` 149);
				it "latin small letter v with hook should be IPA Number 150"
					(latin_small_letter_v_with_hook `shouldBe` 150);
				it "latin small letter turned r should be IPA Number 151"
					(latin_small_letter_turned_r `shouldBe` 151);
				it "latin small letter turned r with hook should be IPA Number 152"
					(latin_small_letter_turned_r_with_hook `shouldBe` 152);
				it "latin small letter j should be IPA Number 153"
					(latin_small_letter_j `shouldBe` 153);
				it "latin small letter turned m with long leg should be IPA Number 154"
					(latin_small_letter_turned_m_with_long_leg `shouldBe` 154);
				it "latin small letter l should be IPA Number 155"
					(latin_small_letter_l `shouldBe` 155);
				it "latin small letter l with retroflex hook should be IPA Number 156"
					(latin_small_letter_l_with_retroflex_hook `shouldBe` 156);
				it "latin small letter turned y should be IPA Number 157"
					(latin_small_letter_turned_y `shouldBe` 157);
				it "latin letter small capital l should be IPA Number 158"
					(latin_letter_small_capital_l `shouldBe` 158);
				it "latin letter bilabial click should be IPA Number 176"
					(latin_letter_bilabial_click `shouldBe` 176);
				it "latin letter dental click should be IPA Number 177"
					(latin_letter_dental_click `shouldBe` 177);
				it "latin letter retroflex click should be IPA Number 178" -- what is this?
					(latin_letter_retroflex_click `shouldBe` 178);
				it "latin letter alveolar click should be IPA Number 179" -- what is this?
					(latin_letter_alveolar_click `shouldBe` 179);
				it "latin letter lateral click should be IPA Number 180"
					(latin_letter_lateral_click `shouldBe` 180);
				it "latin small letter b with hook should be IPA Number 160"
					(latin_small_letter_b_with_hook `shouldBe` 160);
				it "latin small letter d with hook should be IPA Number 162"
					(latin_small_letter_d_with_hook `shouldBe` 162);
				it "latin small letter dotless j with stroke and hook should be IPA Number 164"
					(latin_small_letter_dotless_j_with_stroke_and_hook `shouldBe` 164);
				it "latin small letter g with hook should be IPA Number 166"
					(latin_small_letter_g_with_hook `shouldBe` 166);
				it "latin letter small capital g with hook should be IPA Number 168"
					(latin_letter_small_capital_g_with_hook `shouldBe` 168);
				it "modifier letter apostrophe should be IPA Number 401"
					(modifier_letter_apostrophe `shouldBe` 401);
				it "latin small letter i should be IPA Number 301"
					(latin_small_letter_i `shouldBe` 301);
				it "latin small letter y should be IPA Number 309"
					(latin_small_letter_y `shouldBe` 309);
				it "latin small letter i with stroke should be IPA Number 317"
					(latin_small_letter_i_with_stroke `shouldBe` 317);
				it "latin small letter u bar should be IPA Number 318"
					(latin_small_letter_u_bar `shouldBe` 318);
				it "latin small letter turned m should be IPA Number 316"
					(latin_small_letter_turned_m `shouldBe` 316);
				it "latin small letter u should be IPA Number 308"
					(latin_small_letter_u `shouldBe` 308);
				it "latin letter small capital i should be IPA Number 319"
					(latin_letter_small_capital_i `shouldBe` 319);
				it "latin letter small capital y should be IPA Number 320"
					(latin_letter_small_capital_y `shouldBe` 320);
				it "latin small letter upsilon should be IPA Number 321"
					(latin_small_letter_upsilon `shouldBe` 321);
				it "latin small letter e should be IPA Number 302"
					(latin_small_letter_e `shouldBe` 302);
				it "latin small letter o with stroke should be IPA Number 310"
					(latin_small_letter_o_with_stroke `shouldBe` 310);
				it "latin small letter reversed e should be IPA Number 397"
					(latin_small_letter_reversed_e `shouldBe` 397);
				it "latin small letter barred o should be IPA Number 323"
					(latin_small_letter_barred_o `shouldBe` 323);
				it "latin small letter rams horn should be IPA Number 315"
					(latin_small_letter_rams_horn `shouldBe` 315);
				it "latin small letter o should be IPA Number 307"
					(latin_small_letter_o `shouldBe` 307);
				it "latin small letter schwa should be IPA Number 322"
					(latin_small_letter_schwa `shouldBe` 322);
				it "latin small letter open e should be IPA Number 303"
					(latin_small_letter_open_e `shouldBe` 303);
				it "latin small ligature oe should be IPA Number 311"
					(latin_small_ligature_oe `shouldBe` 311);
				it "latin small letter reversed open e should be IPA Number 326"
					(latin_small_letter_reversed_open_e `shouldBe` 326);
				it "latin small letter closed reversed open e should be IPA Number 395"
					(latin_small_letter_closed_reversed_open_e `shouldBe` 395);
				it "latin small letter turned v should be IPA Number 314"
					(latin_small_letter_turned_v `shouldBe` 314);
				it "latin small letter open o should be IPA Number 306"
					(latin_small_letter_open_o `shouldBe` 306);
				it "latin small letter ae should be IPA Number 325"
					(latin_small_letter_ae `shouldBe` 325);
				it "latin small letter turned a should be IPA Number 324"
					(latin_small_letter_turned_a `shouldBe` 324);
				it "latin small letter a should be IPA Number 304"
					(latin_small_letter_a `shouldBe` 304);
				it "latin letter small capital oe should be IPA Number 312"
					(latin_letter_small_capital_oe `shouldBe` 312);
				it "latin small letter alpha should be IPA Number 305"
					(latin_small_letter_alpha `shouldBe` 305);
				it "latin small letter turned alpha should be IPA Number 313"
					(latin_small_letter_turned_alpha `shouldBe` 313);
				it "latin small letter turned w should be IPA Number 169"
					(latin_small_letter_turned_w `shouldBe` 169);
				it "latin small letter w should be IPA Number 170"
					(latin_small_letter_w `shouldBe` 170);
				it "latin small letter turned h should be IPA Number 171"
					(latin_small_letter_turned_h `shouldBe` 171);
				it "latin letter small capital h should be IPA Number 172"
					(latin_letter_small_capital_h `shouldBe` 172);
				it "latin letter reversed glottal stop with stroke should be IPA Number 174"
					(latin_letter_reversed_glottal_stop_with_stroke `shouldBe` 174);
				it "latin letter glottal stop with stroke should be IPA Number 173"
					(latin_letter_glottal_stop_with_stroke `shouldBe` 173);
				it "latin small letter c with curl should be IPA Number 182"
					(latin_small_letter_c_with_curl `shouldBe` 182);
				it "latin small letter z with curl should be IPA Number 183"
					(latin_small_letter_z_with_curl `shouldBe` 183);
				it "latin small letter turned r with long leg should be IPA Number 181"
					(latin_small_letter_turned_r_with_long_leg `shouldBe` 181);
				it "latin small letter heng with hook should be IPA Number 175"
					(latin_small_letter_heng_with_hook `shouldBe` 175);
				it "combining double inverted breve should be IPA Number 433"
					(combining_double_inverted_breve `shouldBe` 433);
				it "combining double breve below should be IPA Number 433"
					(combining_double_breve_below `shouldBe` 433);
				it "modifier letter vertical line should be IPA Number 501"
					(modifier_letter_vertical_line `shouldBe` 501);
				it "modifier letter low vertical line should be IPA Number 502"
					(modifier_letter_low_vertical_line `shouldBe` 502);
				it "modifier letter triangular colon should be IPA Number 503"
					(modifier_letter_triangular_colon `shouldBe` 503);
				it "modifier letter half triangular colon should be IPA Number 504"
					(modifier_letter_half_triangular_colon `shouldBe` 504);
				it "combining breve should be IPA Number 505"
					(combining_breve `shouldBe` 505);
				it "vertical line should be IPA Number 507"
					(vertical_line `shouldBe` 507);
				it "double vertical line should be IPA Number 508"
					(double_vertical_line `shouldBe` 508);
				it "full stop should be IPA Number 506"
					(full_stop `shouldBe` 506);
				it "undertie should be IPA Number 509"
					(undertie `shouldBe` 509);
				it "combining ring (above or below) should be IPA Number 402"
					(combining_ring_below `shouldBe` 402);

				it "combining caron below should be IPA Number 403"
					(combining_caron_below `shouldBe` 403);
				it "modifier letter small h should be IPA Number 404"
					(modifier_letter_small_h `shouldBe` 404);
				it "combining right half ring below should be IPA Number 411"
					(combining_right_half_ring_below `shouldBe` 411);
				it "combining left half ring below should be IPA Number 412"
					(combining_left_half_ring_below `shouldBe` 412);
				it "combining plus sign below should be IPA Number 413"
					(combining_plus_sign_below `shouldBe` 413);
				it "combining minus sign below should be IPA Number 414"
					(combining_minus_sign_below `shouldBe` 414);
				it "combining diaeresis should be IPA Number 415"
					(combining_diaeresis `shouldBe` 415);
				it "combining x above should be IPA Number 416"
					(combining_x_above `shouldBe` 416);

				it "combining vertical line (above or below) should be IPA Number 431"
					(combining_vertical_line_above `shouldBe` 431);
				it "combining inverted breve below should be IPA Number 432"
					(combining_inverted_breve_below `shouldBe` 432);
				it "modifier letter rhotic hook should be IPA Number 327"
					(modifier_letter_rhotic_hook `shouldBe` 327);
				it "combining diaeresis below should be IPA Number 405"
					(combining_diaeresis_below `shouldBe` 405);
				it "combining tilde below should be IPA Number 406"
					(combining_tilde_below `shouldBe` 406);
				it "combining seagul below should be IPA Number 407"
					(combining_seagul_below `shouldBe` 407);
				it "modifier letter small w should be IPA Number 420"
					(modifier_letter_small_w `shouldBe` 420);
				it "modifier letter small j should be IPA Number 421"
					(modifier_letter_small_j `shouldBe` 421);
				it "modifier letter small gamma should be IPA Number 422"
					(modifier_letter_small_gamma `shouldBe` 422);
				it "modifier letter small reversed glottal stop should be IPA Number 423"
					(modifier_letter_small_reversed_glottal_stop `shouldBe` 423);
				it "combining tilde overlay should be IPA Number 209"
					(combining_tilde_overlay `shouldBe` 428);
				it "combining up tack below should be IPA Number 429"
					(combining_up_tack_below `shouldBe` 429);
				it "combining down tack below should be IPA Number 430"
					(combining_down_tack_below `shouldBe` 430);
				it "combining left tack below should be IPA Number 417"
					(combining_left_tack_below `shouldBe` 417);
				it "combining right tack below should be IPA Number 418"
					(combining_right_tack_below `shouldBe` 418);
				it "combining bridge below should be IPA Number 408"
					(combining_bridge_below `shouldBe` 408);
				it "combining inverted bridge below should be IPA Number 409"
					(combining_inverted_bridge_below `shouldBe` 409);
				it "combining square below should be IPA Number 410"
					(combining_square_below `shouldBe` 410);
				it "combining tilde should be IPA Number 424"
					(combining_tilde `shouldBe` 424);
				it "superscript latin small letter n should be IPA Number 425"
					(superscript_latin_small_letter_n `shouldBe` 425);
				it "modifier letter small l should be IPA Number 426"
					(modifier_letter_small_l `shouldBe` 426);
				it "combining left angle above should be IPA Number 427"
					(combining_left_angle_above `shouldBe` 427);
				it "combining double acute accent should be IPA Number 512"
					(combining_double_acute_accent `shouldBe` 512);
				it "combining acute accent should be IPA Number 513"
					(combining_acute_accent `shouldBe` 513);
				it "combining macron should be IPA Number 514"
					(combining_macron `shouldBe` 514);
				it "combining grave accent should be IPA Number 515"
					(combining_grave_accent `shouldBe` 515);
				it "combining double grave accent should be IPA Number 516"
					(combining_double_grave_accent `shouldBe` 516);
				it "combining caron should be IPA Number 524"
					(combining_caron `shouldBe` 524);
				it "combining circumflex accent should be IPA Number 525"
					(combining_circumflex_accent `shouldBe` 525);
				it "combining macron acute should be IPA Number 526"
					(combining_macron_acute `shouldBe` 526);
				it "combining grave macron should be IPA Number 527"
					(combining_grave_macron `shouldBe` 527);
				it "combining grave acute grave should be IPA Number 528" 
					(combining_grave_acute_grave `shouldBe` 528);
				it "north east arrow should be IPA Number 510"
					(north_east_arrow `shouldBe` 510);
				it "south east arrow should be IPA Number 511"
					(south_east_arrow `shouldBe` 511);
				it "downwards arrow should be IPA Number 517"
					(downwards_arrow `shouldBe` 517);
				it "upwards arrow should be IPA Number 518"
					(upwards_arrow `shouldBe` 518);
				it "modifier letter extra high tone bar should be IPA Number 519"
					(modifier_letter_extra_high_tone_bar `shouldBe` 519);
				it "modifier letter high tone bar should be IPA Number 520"
					(modifier_letter_high_tone_bar `shouldBe` 520);
				it "modifier letter mid tone bar should be IPA Number 521"
					(modifier_letter_mid_tone_bar `shouldBe` 521);
				it "modifier letter low tone bar should be IPA Number 522"
					(modifier_letter_low_tone_bar `shouldBe` 522);
				it "modifier letter extra low tone bar should be IPA Number 523"
					(modifier_letter_extra_low_tone_bar `shouldBe` 523);
			}
		)