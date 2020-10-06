module IPATextToDescriptionSpec where

import Test.Hspec    (Spec, describe, hspec, it, shouldBe)

import DescribeIPASpec.Approximant
import DescribeIPASpec.Fricative
import DescribeIPASpec.LateralApproximant
import DescribeIPASpec.Nasal
import DescribeIPASpec.Plosive
import DescribeIPASpec.TapOrFlap
import DescribeIPASpec.Trill

import qualified IPA 
import Data.Text (pack, unpack)
describeIPA = unpack . IPA.describeIPA . pack

pulmonicEgressiveConsonantSpec = do
  approximantConsonantSpec
  fricativeConsonantSpec
  lateralApproximantConsonantSpec
  nasalConsonantSpec
  plosiveConsonantSpec
  tapOrFlapConsonantSpec
  trillConsonantSpec