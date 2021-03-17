-- | The IPA Numbers module contains definitions for IPA Numbers.
-- For every symbol of the, International Phonetic Alphabet,
-- the international Phonetic Association has
-- associated a positive integer for use by computers.
-- The value of the integers has been taken from
-- the website of the International Phonetic Association
-- at https://www.internationalphoneticassociation.org .
-- Specifically, the values are taken from the following file called the "IPA Number Chart (revised to 2005)"
-- https://www.internationalphoneticassociation.org/sites/default/files/IPA_Number_chart_(C)2005.pdf

-- Note that the name of the identifiers is the name used
-- in Unicode charts. This is the best way to have consistent names.
-- Unicode charts may be found at http://unicode.org/charts/ .
-- Note that all IPA Characters are encoded within the Latin charts.
module IPAConstants.IPANumbers where

import Prelude (Int)

latin_small_letter_p :: Int
latin_small_letter_p = 101 -- p

latin_small_letter_b :: Int
latin_small_letter_b = 102 -- b

latin_small_letter_t :: Int
latin_small_letter_t = 103 -- t

latin_small_letter_d :: Int
latin_small_letter_d = 104 -- d
latin_small_letter_t_with_retroflex_hook :: Int
latin_small_letter_t_with_retroflex_hook = 105 -- ʈ
latin_small_letter_d_with_tail :: Int
latin_small_letter_d_with_tail = 106 -- ɖ
latin_small_letter_c :: Int
latin_small_letter_c = 107 -- c
latin_small_letter_dotless_j_with_stroke :: Int
latin_small_letter_dotless_j_with_stroke = 108 -- ɟ
latin_small_letter_k :: Int
latin_small_letter_k = 109 -- k
latin_small_letter_g :: Int
latin_small_letter_g = 110
latin_small_letter_script_g :: Int
latin_small_letter_script_g = 110
latin_small_letter_q :: Int
latin_small_letter_q = 111 -- q
latin_letter_small_capital_g :: Int
latin_letter_small_capital_g = 112 -- ɢ
latin_letter_glottal_stop :: Int
latin_letter_glottal_stop = 113 -- ʔ
latin_small_letter_m :: Int
latin_small_letter_m = 114 -- m
latin_small_letter_m_with_hook :: Int
latin_small_letter_m_with_hook = 115 -- ɱ
latin_small_letter_n :: Int
latin_small_letter_n = 116 -- n
latin_small_letter_n_with_retroflex_hook :: Int
latin_small_letter_n_with_retroflex_hook = 117 -- ɳ
latin_small_letter_n_with_left_hook :: Int
latin_small_letter_n_with_left_hook = 118 -- ɲ
latin_small_letter_eng :: Int
latin_small_letter_eng = 119 -- ŋ
latin_letter_small_capital_n :: Int
latin_letter_small_capital_n = 120 -- ɴ
latin_letter_small_capital_b :: Int
latin_letter_small_capital_b = 121 -- ʙ
latin_small_letter_r :: Int
latin_small_letter_r = 122 -- r
latin_letter_small_capital_r :: Int
latin_letter_small_capital_r = 123 -- ʀ
latin_small_letter_v_with_right_hook :: Int
latin_small_letter_v_with_right_hook = 184 -- ⱱ
latin_small_letter_r_with_fishhook :: Int
latin_small_letter_r_with_fishhook = 124 -- ɾ
latin_small_letter_r_with_tail :: Int
latin_small_letter_r_with_tail = 125 -- ɽ
latin_small_letter_phi :: Int
latin_small_letter_phi = 126 -- ɸ
greek_small_letter_beta :: Int
greek_small_letter_beta = 127 -- β
latin_small_letter_f :: Int
latin_small_letter_f = 128 -- f
latin_small_letter_v :: Int
latin_small_letter_v = 129 -- v
greek_small_letter_theta :: Int
greek_small_letter_theta = 130 -- θ
latin_small_letter_eth :: Int
latin_small_letter_eth = 131 -- ð
latin_small_letter_s :: Int
latin_small_letter_s = 132 -- s
latin_small_letter_z :: Int
latin_small_letter_z = 133 -- z
latin_small_letter_esh :: Int
latin_small_letter_esh = 134 -- ʃ
latin_small_letter_ezh :: Int
latin_small_letter_ezh = 135 -- ʒ
latin_small_letter_s_with_hook :: Int
latin_small_letter_s_with_hook = 136 -- ʂ
latin_small_letter_z_with_retroflex_hook :: Int
latin_small_letter_z_with_retroflex_hook = 137 -- ʐ
latin_small_letter_c_with_cedilla :: Int
latin_small_letter_c_with_cedilla = 138 -- ç
latin_small_letter_j_with_crossed_tail :: Int
latin_small_letter_j_with_crossed_tail = 139 -- ʝ
latin_small_letter_x :: Int
latin_small_letter_x = 140 -- x
latin_small_letter_gamma :: Int
latin_small_letter_gamma = 141 -- ɣ
greek_small_letter_chi :: Int
greek_small_letter_chi = 142 -- χ
latin_letter_small_capital_inverted_r :: Int
latin_letter_small_capital_inverted_r = 143 -- ʁ
latin_small_letter_h_with_stroke :: Int
latin_small_letter_h_with_stroke = 144 -- ħ
latin_letter_pharyngeal_voiced_fricative :: Int
latin_letter_pharyngeal_voiced_fricative = 145 -- ʕ
latin_small_letter_h :: Int
latin_small_letter_h = 146 -- h
latin_small_letter_h_with_hook :: Int
latin_small_letter_h_with_hook = 147 -- ɦ
latin_small_letter_l_with_belt :: Int
latin_small_letter_l_with_belt = 148 -- ɬ
latin_small_letter_lezh :: Int
latin_small_letter_lezh = 149 -- ɮ
latin_small_letter_v_with_hook :: Int
latin_small_letter_v_with_hook = 150 -- ʋ
latin_small_letter_turned_r :: Int
latin_small_letter_turned_r = 151 -- ɹ
latin_small_letter_turned_r_with_hook :: Int
latin_small_letter_turned_r_with_hook = 152 -- ɻ
latin_small_letter_j :: Int
latin_small_letter_j = 153 -- j
latin_small_letter_turned_m_with_long_leg :: Int
latin_small_letter_turned_m_with_long_leg = 154 -- ɰ
latin_small_letter_l :: Int
latin_small_letter_l = 155 -- l
latin_small_letter_l_with_retroflex_hook :: Int
latin_small_letter_l_with_retroflex_hook = 156 -- ɭ
latin_small_letter_turned_y :: Int
latin_small_letter_turned_y = 157 -- ʎ
latin_letter_small_capital_l :: Int
latin_letter_small_capital_l = 158 -- ʟ
latin_letter_bilabial_click :: Int
latin_letter_bilabial_click = 176 -- ʘ
latin_letter_dental_click :: Int
latin_letter_dental_click = 177 -- ǀ
latin_letter_retroflex_click :: Int
latin_letter_retroflex_click = 178 -- ǃ
latin_letter_alveolar_click :: Int
latin_letter_alveolar_click = 179 -- ǂ
latin_letter_lateral_click :: Int
latin_letter_lateral_click = 180 -- ǁ
latin_small_letter_b_with_hook :: Int
latin_small_letter_b_with_hook = 160 -- ɓ
latin_small_letter_d_with_hook :: Int
latin_small_letter_d_with_hook = 162 -- ɗ
latin_small_letter_dotless_j_with_stroke_and_hook :: Int
latin_small_letter_dotless_j_with_stroke_and_hook = 164 -- ʄ
latin_small_letter_g_with_hook :: Int
latin_small_letter_g_with_hook = 166 -- ɠ
latin_letter_small_capital_g_with_hook :: Int
latin_letter_small_capital_g_with_hook = 168 -- ʛ
modifier_letter_apostrophe :: Int
modifier_letter_apostrophe = 401 -- ʼ
latin_small_letter_i :: Int
latin_small_letter_i = 301 -- i
latin_small_letter_y :: Int
latin_small_letter_y = 309 -- y
latin_small_letter_i_with_stroke :: Int
latin_small_letter_i_with_stroke = 317 -- ɨ
latin_small_letter_u_bar :: Int
latin_small_letter_u_bar = 318 -- ʉ
latin_small_letter_turned_m :: Int
latin_small_letter_turned_m = 316 -- ɯ
latin_small_letter_u :: Int
latin_small_letter_u = 308 -- u
latin_letter_small_capital_i :: Int
latin_letter_small_capital_i = 319 -- ɪ
latin_letter_small_capital_y :: Int
latin_letter_small_capital_y = 320 -- ʏ
latin_small_letter_upsilon :: Int
latin_small_letter_upsilon = 321 -- ʊ
latin_small_letter_e :: Int
latin_small_letter_e = 302 -- e
latin_small_letter_o_with_stroke :: Int
latin_small_letter_o_with_stroke = 310 -- ø
latin_small_letter_reversed_e :: Int
latin_small_letter_reversed_e = 397 -- ɘ
latin_small_letter_barred_o :: Int
latin_small_letter_barred_o = 323 -- ɵ
latin_small_letter_rams_horn :: Int
latin_small_letter_rams_horn = 315 -- ɤ
latin_small_letter_o :: Int
latin_small_letter_o = 307 -- o
latin_small_letter_schwa :: Int
latin_small_letter_schwa = 322 -- ə
latin_small_letter_open_e :: Int
latin_small_letter_open_e = 303 -- ɛ
latin_small_ligature_oe :: Int
latin_small_ligature_oe = 311 -- œ
latin_small_letter_reversed_open_e :: Int
latin_small_letter_reversed_open_e = 326 -- ɜ
latin_small_letter_closed_reversed_open_e :: Int
latin_small_letter_closed_reversed_open_e = 395 -- ɞ
latin_small_letter_turned_v :: Int
latin_small_letter_turned_v = 314 -- ʌ
latin_small_letter_open_o :: Int
latin_small_letter_open_o = 306 -- ɔ
latin_small_letter_ae :: Int
latin_small_letter_ae = 325 -- æ
latin_small_letter_turned_a :: Int
latin_small_letter_turned_a = 324 -- ɐ

latin_small_letter_a :: Int
latin_small_letter_a = 304 --a
latin_letter_small_capital_oe :: Int
latin_letter_small_capital_oe = 312 -- ɶ
latin_small_letter_alpha :: Int
latin_small_letter_alpha = 305 -- ɑ
latin_small_letter_turned_alpha :: Int
latin_small_letter_turned_alpha = 313 -- ɒ
latin_small_letter_turned_w :: Int
latin_small_letter_turned_w = 169 -- ʍ
latin_small_letter_w :: Int
latin_small_letter_w = 170 -- w
latin_small_letter_turned_h :: Int
latin_small_letter_turned_h = 171 -- ɥ
latin_letter_small_capital_h :: Int
latin_letter_small_capital_h = 172 -- ʜ
latin_letter_reversed_glottal_stop_with_stroke :: Int
latin_letter_reversed_glottal_stop_with_stroke = 174 -- ʢ
latin_letter_glottal_stop_with_stroke :: Int
latin_letter_glottal_stop_with_stroke = 173 -- ʡ
latin_small_letter_c_with_curl :: Int
latin_small_letter_c_with_curl = 182 -- ɕ
latin_small_letter_z_with_curl :: Int
latin_small_letter_z_with_curl = 183 -- ʑ
latin_small_letter_turned_r_with_long_leg :: Int
latin_small_letter_turned_r_with_long_leg = 181 -- ɺ
latin_small_letter_heng_with_hook :: Int
latin_small_letter_heng_with_hook = 175 -- ɧ

combining_double_inverted_breve :: Int
combining_double_inverted_breve = 433 -- k͡p 
combining_double_breve_below :: Int
combining_double_breve_below = 433 -- t͜s  ͡    ͜

modifier_letter_vertical_line :: Int
modifier_letter_vertical_line = 501 -- ˈ
modifier_letter_low_vertical_line :: Int
modifier_letter_low_vertical_line = 502 -- ˌ
modifier_letter_triangular_colon :: Int
modifier_letter_triangular_colon = 503 -- ː
modifier_letter_half_triangular_colon :: Int
modifier_letter_half_triangular_colon = 504 -- ˑ
combining_breve :: Int
combining_breve = 505 -- ĕ
vertical_line :: Int
vertical_line = 507 --   |
double_vertical_line :: Int
double_vertical_line = 508 -- ‖
full_stop :: Int
full_stop = 506 -- .
undertie :: Int
undertie = 509 -- ‿
combining_ring_below :: Int
combining_ring_below = 402 -- ̥
combining_ring_above :: Int
combining_ring_above = 402 -- ̊

combining_caron_below :: Int
combining_caron_below = 403 --  ̬
modifier_letter_small_h :: Int
modifier_letter_small_h = 404 -- ʰ
combining_right_half_ring_below :: Int
combining_right_half_ring_below = 411 -- ̹
combining_left_half_ring_below :: Int
combining_left_half_ring_below = 412 -- ̜
combining_plus_sign_below :: Int
combining_plus_sign_below = 413 -- ̟
combining_minus_sign_below :: Int
combining_minus_sign_below = 414 -- ̠
combining_diaeresis :: Int
combining_diaeresis = 415 -- ̈
combining_x_above :: Int
combining_x_above = 416 -- ̽
combining_vertical_line_above :: Int
combining_vertical_line_above = 431 -- ̍ 
combining_vertical_line_below :: Int
combining_vertical_line_below = 431 -- ̩

-- combining_dot_above :: Int
-- combining_dot_above = '\x0307' -- ̇
-- combining_dot_below :: Int
-- combining_dot_below = '\x0323' --̣

combining_inverted_breve_below :: Int
combining_inverted_breve_below = 432 -- ̯
modifier_letter_rhotic_hook :: Int
modifier_letter_rhotic_hook = 419 -- ˞
combining_diaeresis_below :: Int
combining_diaeresis_below = 405 -- ̤
combining_tilde_below :: Int
combining_tilde_below = 406 -- ̰
combining_seagul_below :: Int
combining_seagul_below = 407 -- ̼
modifier_letter_small_w :: Int
modifier_letter_small_w = 420 -- ʷ
modifier_letter_small_j :: Int
modifier_letter_small_j = 421 -- ʲ
modifier_letter_small_gamma :: Int
modifier_letter_small_gamma = 422 -- ˠ
modifier_letter_small_reversed_glottal_stop :: Int
modifier_letter_small_reversed_glottal_stop = 423 -- ˤ
combining_tilde_overlay :: Int
combining_tilde_overlay = 428 --  ̴
combining_up_tack_below :: Int
combining_up_tack_below = 429 -- ̝
combining_down_tack_below :: Int
combining_down_tack_below = 430 -- ̞
combining_left_tack_below :: Int
combining_left_tack_below = 417 -- ̘
combining_right_tack_below :: Int
combining_right_tack_below = 418 -- ̙
combining_bridge_below :: Int
combining_bridge_below = 408 -- ̪
combining_inverted_bridge_below :: Int
combining_inverted_bridge_below = 409 -- ̺
combining_square_below :: Int
combining_square_below = 410 -- ̻
combining_tilde :: Int
combining_tilde = 424 -- ̃
superscript_latin_small_letter_n :: Int
superscript_latin_small_letter_n = 425 -- ⁿ
modifier_letter_small_l :: Int
modifier_letter_small_l = 426 -- ˡ
combining_left_angle_above :: Int
combining_left_angle_above = 427 -- ̚
combining_double_acute_accent :: Int
combining_double_acute_accent = 512 -- ̋
combining_acute_accent :: Int
combining_acute_accent = 513 -- ́
combining_macron :: Int
combining_macron = 514 -- ̄
combining_grave_accent :: Int
combining_grave_accent = 515 -- ̀
combining_double_grave_accent :: Int
combining_double_grave_accent = 516 -- ̏
combining_caron :: Int
combining_caron = 524 -- ̌
combining_circumflex_accent :: Int
combining_circumflex_accent = 525 -- ̂
combining_macron_acute :: Int
combining_macron_acute = 526 -- ᷄
combining_grave_macron :: Int
combining_grave_macron = 527 -- ᷅
combining_grave_acute_grave :: Int
combining_grave_acute_grave = 528 -- ᷈
north_east_arrow :: Int
north_east_arrow = 510 -- ↗
south_east_arrow :: Int
south_east_arrow = 511 -- ↘
downwards_arrow :: Int
downwards_arrow = 517 -- ↓
upwards_arrow :: Int
upwards_arrow = 518 -- ↑
modifier_letter_extra_high_tone_bar :: Int
modifier_letter_extra_high_tone_bar = 519 -- ˥
modifier_letter_high_tone_bar :: Int
modifier_letter_high_tone_bar = 520 -- ˦
modifier_letter_mid_tone_bar :: Int
modifier_letter_mid_tone_bar = 521 -- ˧
modifier_letter_low_tone_bar :: Int
modifier_letter_low_tone_bar = 522 -- ˨
modifier_letter_extra_low_tone_bar :: Int
modifier_letter_extra_low_tone_bar = 523 -- ˩


