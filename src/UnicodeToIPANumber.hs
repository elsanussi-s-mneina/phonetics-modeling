module UnicodeToIPANumber where

import Prelude (Int, Char, (==))

import qualified IPAConstants.IPANumbers as Numbers
import IPAConstants.IPAUnicodeConstants


unicodeToNumber :: Char -> Int
unicodeToNumber character =
	case character of
	{
		_ | character == latin_small_letter_p -> Numbers.latin_small_letter_p;
		_ | character == latin_small_letter_b -> Numbers.latin_small_letter_b;
		_ | character == latin_small_letter_t -> Numbers.latin_small_letter_t;
		_ | character == latin_small_letter_d -> Numbers.latin_small_letter_d;
		_ | character == latin_small_letter_t_with_retroflex_hook -> Numbers.latin_small_letter_t_with_retroflex_hook;
		_ | character == latin_small_letter_d_with_tail -> Numbers.latin_small_letter_d_with_tail;
		_ | character == latin_small_letter_c -> Numbers.latin_small_letter_c;
		_ | character == latin_small_letter_dotless_j_with_stroke -> Numbers.latin_small_letter_dotless_j_with_stroke;
		_ | character == latin_small_letter_k -> Numbers.latin_small_letter_k;
		_ | character == latin_small_letter_g -> Numbers.latin_small_letter_g;
		_ | character == latin_small_letter_script_g -> Numbers.latin_small_letter_script_g;
		_ | character == latin_small_letter_q -> Numbers.latin_small_letter_q;
		_ | character == latin_letter_small_capital_g -> Numbers.latin_letter_small_capital_g;
		_ | character == latin_letter_glottal_stop -> Numbers.latin_letter_glottal_stop;
		_ | character == latin_small_letter_m -> Numbers.latin_small_letter_m;
		_ | character == latin_small_letter_m_with_hook -> Numbers.latin_small_letter_m_with_hook;
		_ | character == latin_small_letter_n -> Numbers.latin_small_letter_n;
		_ | character == latin_small_letter_n_with_retroflex_hook -> Numbers.latin_small_letter_n_with_retroflex_hook;
		_ | character == latin_small_letter_n_with_left_hook -> Numbers.latin_small_letter_n_with_left_hook;
		_ | character == latin_small_letter_eng -> Numbers.latin_small_letter_eng;
		_ | character == latin_letter_small_capital_n -> Numbers.latin_letter_small_capital_n;
		_ | character == latin_letter_small_capital_b -> Numbers.latin_letter_small_capital_b;
		_ | character == latin_small_letter_r -> Numbers.latin_small_letter_r;
		_ | character == latin_letter_small_capital_r -> Numbers.latin_letter_small_capital_r;
		_ | character == latin_small_letter_v_with_right_hook -> Numbers.latin_small_letter_v_with_right_hook;
		_ | character == latin_small_letter_r_with_fishhook -> Numbers.latin_small_letter_r_with_fishhook;
		_ | character == latin_small_letter_r_with_tail -> Numbers.latin_small_letter_r_with_tail;
		_ | character == latin_small_letter_phi -> Numbers.latin_small_letter_phi;
		_ | character == greek_small_letter_beta -> Numbers.greek_small_letter_beta;
		_ | character == latin_small_letter_f -> Numbers.latin_small_letter_f;
		_ | character == latin_small_letter_v -> Numbers.latin_small_letter_v;
		_ | character == greek_small_letter_theta -> Numbers.greek_small_letter_theta;
		_ | character == latin_small_letter_eth -> Numbers.latin_small_letter_eth;
		_ | character == latin_small_letter_s -> Numbers.latin_small_letter_s;
		_ | character == latin_small_letter_z -> Numbers.latin_small_letter_z;
		_ | character == latin_small_letter_esh -> Numbers.latin_small_letter_esh;
		_ | character == latin_small_letter_ezh -> Numbers.latin_small_letter_ezh;
		_ | character == latin_small_letter_s_with_hook -> Numbers.latin_small_letter_s_with_hook;
		_ | character == latin_small_letter_z_with_retroflex_hook -> Numbers.latin_small_letter_z_with_retroflex_hook;
		_ | character == latin_small_letter_c_with_cedilla -> Numbers.latin_small_letter_c_with_cedilla;
		_ | character == latin_small_letter_j_with_crossed_tail -> Numbers.latin_small_letter_j_with_crossed_tail;
		_ | character == latin_small_letter_x -> Numbers.latin_small_letter_x;
		_ | character == latin_small_letter_gamma -> Numbers.latin_small_letter_gamma;
		_ | character == greek_small_letter_chi -> Numbers.greek_small_letter_chi;
		_ | character == latin_letter_small_capital_inverted_r -> Numbers.latin_letter_small_capital_inverted_r;
		_ | character == latin_small_letter_h_with_stroke -> Numbers.latin_small_letter_h_with_stroke;
		_ | character == latin_letter_pharyngeal_voiced_fricative -> Numbers.latin_letter_pharyngeal_voiced_fricative;
		_ | character == latin_small_letter_h -> Numbers.latin_small_letter_h;
		_ | character == latin_small_letter_h_with_hook -> Numbers.latin_small_letter_h_with_hook;
		_ | character == latin_small_letter_l_with_belt -> Numbers.latin_small_letter_l_with_belt;
		_ | character == latin_small_letter_lezh -> Numbers.latin_small_letter_lezh;
		_ | character == latin_small_letter_v_with_hook -> Numbers.latin_small_letter_v_with_hook;
		_ | character == latin_small_letter_turned_r -> Numbers.latin_small_letter_turned_r;
		_ | character == latin_small_letter_turned_r_with_hook -> Numbers.latin_small_letter_turned_r_with_hook;
		_ | character == latin_small_letter_j -> Numbers.latin_small_letter_j;
		_ | character == latin_small_letter_turned_m_with_long_leg -> Numbers.latin_small_letter_turned_m_with_long_leg;
		_ | character == latin_small_letter_l -> Numbers.latin_small_letter_l;
		_ | character == latin_small_letter_l_with_retroflex_hook -> Numbers.latin_small_letter_l_with_retroflex_hook;
		_ | character == latin_small_letter_turned_y -> Numbers.latin_small_letter_turned_y;
		_ | character == latin_letter_small_capital_l -> Numbers.latin_letter_small_capital_l;
		_ | character == latin_letter_bilabial_click -> Numbers.latin_letter_bilabial_click;
		_ | character == latin_letter_dental_click -> Numbers.latin_letter_dental_click;
		_ | character == latin_letter_retroflex_click -> Numbers.latin_letter_retroflex_click;
		_ | character == latin_letter_alveolar_click -> Numbers.latin_letter_alveolar_click;
		_ | character == latin_letter_lateral_click -> Numbers.latin_letter_lateral_click;
		_ | character == latin_small_letter_b_with_hook -> Numbers.latin_small_letter_b_with_hook;
		_ | character == latin_small_letter_d_with_hook -> Numbers.latin_small_letter_d_with_hook;
		_ | character == latin_small_letter_dotless_j_with_stroke_and_hook -> Numbers.latin_small_letter_dotless_j_with_stroke_and_hook;
		_ | character == latin_small_letter_g_with_hook -> Numbers.latin_small_letter_g_with_hook;
		_ | character == latin_letter_small_capital_g_with_hook -> Numbers.latin_letter_small_capital_g_with_hook;
		_ | character == modifier_letter_apostrophe -> Numbers.modifier_letter_apostrophe;
		_ | character == latin_small_letter_i -> Numbers.latin_small_letter_i;
		_ | character == latin_small_letter_y -> Numbers.latin_small_letter_y;
		_ | character == latin_small_letter_i_with_stroke -> Numbers.latin_small_letter_i_with_stroke;
		_ | character == latin_small_letter_u_bar -> Numbers.latin_small_letter_u_bar;
		_ | character == latin_small_letter_turned_m -> Numbers.latin_small_letter_turned_m;
		_ | character == latin_small_letter_u -> Numbers.latin_small_letter_u;
		_ | character == latin_letter_small_capital_i -> Numbers.latin_letter_small_capital_i;
		_ | character == latin_letter_small_capital_y -> Numbers.latin_letter_small_capital_y;
		_ | character == latin_small_letter_upsilon -> Numbers.latin_small_letter_upsilon;
		_ | character == latin_small_letter_e -> Numbers.latin_small_letter_e;
		_ | character == latin_small_letter_o_with_stroke -> Numbers.latin_small_letter_o_with_stroke;
		_ | character == latin_small_letter_reversed_e -> Numbers.latin_small_letter_reversed_e;
		_ | character == latin_small_letter_barred_o -> Numbers.latin_small_letter_barred_o;
		_ | character == latin_small_letter_rams_horn -> Numbers.latin_small_letter_rams_horn;
		_ | character == latin_small_letter_o -> Numbers.latin_small_letter_o;
		_ | character == latin_small_letter_schwa -> Numbers.latin_small_letter_schwa;
		_ | character == latin_small_letter_open_e -> Numbers.latin_small_letter_open_e;
		_ | character == latin_small_ligature_oe -> Numbers.latin_small_ligature_oe;
		_ | character == latin_small_letter_reversed_open_e -> Numbers.latin_small_letter_reversed_open_e;
		_ | character == latin_small_letter_closed_reversed_open_e -> Numbers.latin_small_letter_closed_reversed_open_e;
		_ | character == latin_small_letter_turned_v -> Numbers.latin_small_letter_turned_v;
		_ | character == latin_small_letter_open_o -> Numbers.latin_small_letter_open_o;
		_ | character == latin_small_letter_ae -> Numbers.latin_small_letter_ae;
		_ | character == latin_small_letter_turned_a -> Numbers.latin_small_letter_turned_a;
		_ | character == latin_small_letter_a -> Numbers.latin_small_letter_a;
		_ | character == latin_letter_small_capital_oe -> Numbers.latin_letter_small_capital_oe;
		_ | character == latin_small_letter_alpha -> Numbers.latin_small_letter_alpha;
		_ | character == latin_small_letter_turned_alpha -> Numbers.latin_small_letter_turned_alpha;
		_ | character == latin_small_letter_turned_w -> Numbers.latin_small_letter_turned_w;
		_ | character == latin_small_letter_w -> Numbers.latin_small_letter_w;
		_ | character == latin_small_letter_turned_h -> Numbers.latin_small_letter_turned_h;
		_ | character == latin_letter_small_capital_h -> Numbers.latin_letter_small_capital_h;
		_ | character == latin_letter_reversed_glottal_stop_with_stroke -> Numbers.latin_letter_reversed_glottal_stop_with_stroke;
		_ | character == latin_letter_glottal_stop_with_stroke -> Numbers.latin_letter_glottal_stop_with_stroke;
		_ | character == latin_small_letter_c_with_curl -> Numbers.latin_small_letter_c_with_curl;
		_ | character == latin_small_letter_z_with_curl -> Numbers.latin_small_letter_z_with_curl;
		_ | character == latin_small_letter_turned_r_with_long_leg -> Numbers.latin_small_letter_turned_r_with_long_leg;
		_ | character == latin_small_letter_heng_with_hook -> Numbers.latin_small_letter_heng_with_hook;
		_ | character == combining_double_inverted_breve -> Numbers.combining_double_inverted_breve;
		_ | character == combining_double_breve_below -> Numbers.combining_double_breve_below;
		_ | character == modifier_letter_vertical_line -> Numbers.modifier_letter_vertical_line;
		_ | character == modifier_letter_low_vertical_line -> Numbers.modifier_letter_low_vertical_line;
		_ | character == modifier_letter_triangular_colon -> Numbers.modifier_letter_triangular_colon;
		_ | character == modifier_letter_half_triangular_colon -> Numbers.modifier_letter_half_triangular_colon;
		_ | character == combining_breve -> Numbers.combining_breve;
		_ | character == vertical_line -> Numbers.vertical_line;
		_ | character == double_vertical_line -> Numbers.double_vertical_line;
		_ | character == full_stop -> Numbers.full_stop;
		_ | character == undertie -> Numbers.undertie;
		_ | character == combining_ring_below -> Numbers.combining_ring_below;
		_ | character == combining_ring_above -> Numbers.combining_ring_above;
		_ | character == combining_caron_below -> Numbers.combining_caron_below;
		_ | character == modifier_letter_small_h -> Numbers.modifier_letter_small_h;
		_ | character == combining_right_half_ring_below -> Numbers.combining_right_half_ring_below;
		_ | character == combining_left_half_ring_below -> Numbers.combining_left_half_ring_below;
		_ | character == combining_plus_sign_below -> Numbers.combining_plus_sign_below;
		_ | character == combining_minus_sign_below -> Numbers.combining_minus_sign_below;
		_ | character == combining_diaeresis -> Numbers.combining_diaeresis;
		_ | character == combining_x_above -> Numbers.combining_x_above;
		_ | character == combining_vertical_line_above -> Numbers.combining_vertical_line_above;
		_ | character == combining_vertical_line_below -> Numbers.combining_vertical_line_below;
		_ | character == combining_inverted_breve_below -> Numbers.combining_inverted_breve_below;
		_ | character == modifier_letter_rhotic_hook -> Numbers.modifier_letter_rhotic_hook;
		_ | character == combining_diaeresis_below -> Numbers.combining_diaeresis_below;
		_ | character == combining_tilde_below -> Numbers.combining_tilde_below;
		_ | character == combining_seagul_below -> Numbers.combining_seagul_below;
		_ | character == modifier_letter_small_w -> Numbers.modifier_letter_small_w;
		_ | character == modifier_letter_small_j -> Numbers.modifier_letter_small_j;
		_ | character == modifier_letter_small_gamma -> Numbers.modifier_letter_small_gamma;
		_ | character == modifier_letter_small_reversed_glottal_stop -> Numbers.modifier_letter_small_reversed_glottal_stop;
		_ | character == combining_tilde_overlay -> Numbers.combining_tilde_overlay;
		_ | character == combining_up_tack_below -> Numbers.combining_up_tack_below;
		_ | character == combining_down_tack_below -> Numbers.combining_down_tack_below;
		_ | character == combining_left_tack_below -> Numbers.combining_left_tack_below;
		_ | character == combining_right_tack_below -> Numbers.combining_right_tack_below;
		_ | character == combining_bridge_below -> Numbers.combining_bridge_below;
		_ | character == combining_inverted_bridge_below -> Numbers.combining_inverted_bridge_below;
		_ | character == combining_square_below -> Numbers.combining_square_below;
		_ | character == combining_tilde -> Numbers.combining_tilde;
		_ | character == superscript_latin_small_letter_n -> Numbers.superscript_latin_small_letter_n;
		_ | character == modifier_letter_small_l -> Numbers.modifier_letter_small_l;
		_ | character == combining_left_angle_above -> Numbers.combining_left_angle_above;
		_ | character == combining_double_acute_accent -> Numbers.combining_double_acute_accent;
		_ | character == combining_acute_accent -> Numbers.combining_acute_accent;
		_ | character == combining_macron -> Numbers.combining_macron;
		_ | character == combining_grave_accent -> Numbers.combining_grave_accent;
		_ | character == combining_double_grave_accent -> Numbers.combining_double_grave_accent;
		_ | character == combining_caron -> Numbers.combining_caron;
		_ | character == combining_circumflex_accent -> Numbers.combining_circumflex_accent;
		_ | character == combining_macron_acute -> Numbers.combining_macron_acute;
		_ | character == combining_grave_macron -> Numbers.combining_grave_macron;
		_ | character == combining_grave_acute_grave -> Numbers.combining_grave_acute_grave;
		_ | character == north_east_arrow -> Numbers.north_east_arrow;
		_ | character == south_east_arrow -> Numbers.south_east_arrow;
		_ | character == downwards_arrow -> Numbers.downwards_arrow;
		_ | character == upwards_arrow -> Numbers.upwards_arrow;
		_ | character == modifier_letter_extra_high_tone_bar -> Numbers.modifier_letter_extra_high_tone_bar;
		_ | character == modifier_letter_high_tone_bar -> Numbers.modifier_letter_high_tone_bar;
		_ | character == modifier_letter_mid_tone_bar -> Numbers.modifier_letter_mid_tone_bar;
		_ | character == modifier_letter_low_tone_bar -> Numbers.modifier_letter_low_tone_bar;
		_ | character == modifier_letter_extra_low_tone_bar -> Numbers.modifier_letter_extra_low_tone_bar;
		_ -> 0; -- not recognized
	}
