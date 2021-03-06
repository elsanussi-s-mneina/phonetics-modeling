module IPATextToDescriptionSpec where

import Test.Hspec (Spec)
import DescribeIPASpec.Approximant ( approximantConsonantSpec )
import DescribeIPASpec.Fricative ( fricativeConsonantSpec )
import DescribeIPASpec.LateralApproximant
    ( lateralApproximantConsonantSpec )
import DescribeIPASpec.Nasal ( nasalConsonantSpec )
import DescribeIPASpec.Plosive ( plosiveConsonantSpec )
import DescribeIPASpec.TapOrFlap ( tapOrFlapConsonantSpec )
import DescribeIPASpec.Trill ( trillConsonantSpec )
import DescribeIPASpec.VowelNasalization ( nasalVowelSpec )

import qualified IPA 
import Data.Text (pack, unpack)

describeIPA :: String -> String
describeIPA = unpack . IPA.describeIPA . pack

pulmonicEgressiveConsonantSpec :: Spec
pulmonicEgressiveConsonantSpec = do
	approximantConsonantSpec
	fricativeConsonantSpec
	lateralApproximantConsonantSpec
	nasalConsonantSpec
	plosiveConsonantSpec
	tapOrFlapConsonantSpec
	trillConsonantSpec
	nasalVowelSpec