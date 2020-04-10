module Experimental.CodeGeneration.InternationalPhoneticAlphabetCodeGen(codeGenAllStatementsConstructIPAHaskell) where


import Lib 
  ( Phonet
  , Airstream(PulmonicEgressive)
  , UnmarkablePhonet(UnmarkableConsonant)
  , UnmarkableVocalFolds(UnmarkedVocalFolds)
  , UnmarkablePlace(UnmarkedPlace)
  , UnmarkableManner(UnmarkedManner)
  , UnmarkableAirstream(MarkedAirstream)
  , generateFromUnmarked
  )

import Prelude (String, map, show, (++))

import Data.List (intercalate)
import InternationalPhoneticAlphabet (analyzeIPA, constructIPA)

codeGenConstructIPAHaskell :: Phonet -> String 
codeGenConstructIPAHaskell p =
  "constructIPA " ++ show p ++ " = \"" ++ constructIPA p ++ "\""

codeGenAnalyzeIPAHaskell :: String -> String
codeGenAnalyzeIPAHaskell p =
  "analyzeIPA \"" ++ p ++ "\" = " ++ show (analyzeIPA p)

codeGenAllStatementsConstructIPAHaskell :: String
codeGenAllStatementsConstructIPAHaskell =
  let 
    construct_statements = map codeGenConstructIPAHaskell (generateFromUnmarked (UnmarkableConsonant UnmarkedVocalFolds UnmarkedPlace UnmarkedManner (MarkedAirstream PulmonicEgressive)))
    analyze_statements = map codeGenAnalyzeIPAHaskell (map constructIPA (generateFromUnmarked (UnmarkableConsonant UnmarkedVocalFolds UnmarkedPlace UnmarkedManner (MarkedAirstream PulmonicEgressive))))
  in intercalate "\n" (construct_statements ++ analyze_statements)

-- Note: 
-- TODO: create function for generating code for the phonete types. Currently it only
-- calls the show function which is not correct.