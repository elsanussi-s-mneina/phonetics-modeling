{-# LANGUAGE NoImplicitPrelude #-}


module PhoneticFeaturesTypes where
import           Relude  (Eq)


{-|
 Represents the '+' (plus) or '-' (minus)
 of a binary feature. e.g. [+sonorant],
 [-sonorant]
-}
data Polarity = Plus | Minus
                deriving Eq



{-|
 In Linguistics, phonemes can be
 analyzed as a set of features. One phoneme
 will have one set of features, and a different
 phoneme will have a different set of features.

 These features are well known in phonology, and
 are limited in number. There are two kinds of
 features, unary features, and binary features. The
 difference is obvious in how they are represented in
 the notation that linguists use. Unary features,
 are either present or absent. Binary features
 can be positive or negative. For example, Nasal
 is a unary feature. A phoneme is either nasal,
 or it isn't. i.e. [nasal] or not. For example,
 Voice is a binary feature, a phoneme can be
 [+voice] (can be pronounced: "plus voice")
 or [-voice] (can be pronounced: "minus voice").

 Because linguists represent phonemic features in these
 two different ways. We represent these as two
 different kinds of types.

 So [nasal] which is a unary feature would be
 represented by a value `NasalFeature` of type `PhonemeFeature`.
 And [+voice] which is a binary feature would
 be represented by a value `VoiceFeature Plus` of type
 `PhonemeFeature`.

 We represent the plus or minus symbol by
 the type Polarity.

 Notice that: Linguists write a set of features
 as a 2D matrix with one column, roughly like this:
 ⎡ +voice    ⎤
 ⎢ +sonorant ⎥
 ⎣  nasal    ⎦

Note that certain sets of features cannot coexist,
syntactically. For example a phoneme cannot be
[+voice] and [-voice].

 Note that some analyses
are language specific, so for some phonemes (not
the usual case) whether it has feature X (say 'coronal')
depends on the language (theoretical example: e.g. Swahili,
vs French). This is not implemented here.
-}
data PhonemeFeature = SyllabicFeature Polarity
                    | ConsonantalFeature Polarity
                    | SonorantFeature Polarity
                    | ContinuantFeature Polarity
                    | VoiceFeature Polarity
                    | AdvancedTongueRootFeature Polarity
                    | NasalFeature
                    | LateralFeature
                    | DelayedReleaseFeature
                    | SpreadGlottisFeature
                    | ConstrictedGlottisFeature
                    | LabialFeature
                    | CoronalFeature
                    | DorsalFeature
                    | PharyngealFeature
                    | LaryngealFeature
                    | RoundFeature Polarity
                    | AnteriorFeature Polarity
                    | DistributedFeature Polarity
                    | StridentFeature Polarity
                    | HighFeature Polarity
                    | LowFeature Polarity
                    | BackFeature Polarity
                    deriving Eq
