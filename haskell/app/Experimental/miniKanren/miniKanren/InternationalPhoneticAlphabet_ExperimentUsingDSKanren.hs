{-
You need the following dependency to follow along in the examples:

"ds-kanren-0.2.0.1@sha256:b4275541bcc48385d04f8c225509359191a2e0d5faaa078f1cc4e2c52db3d1d8,1527"


I used the command 
stack -- ghci
in the "phonetics-modeling/haskell" directory

(Note: "*..>" is notation here for the prompt in GHCi)
Then run:
*..> :l app/Experimental/miniKanren/miniKanren/InternationalPhoneticAlphabet_ExperimentUsingDSKanren.hs 

Then run main:
*..> main

to check that the code runs.
-}
{-# LANGUAGE UnicodeSyntax #-}

module InternationalPhoneticAlphabet_ExperimentUsingDSKanren (makeVoiced, isVoiced, facts, main, isVoicedO, makeVoicedO, makeVoicelessO, makeVoiceless) where 

import Prelude (Bool, IO, String, foldr, fst, head, length, not, null, print, (>), ($))
import Prelude.Unicode ((∘), (⧺))
import Language.DSKanren (Term(Atom), Predicate, conde, disconj, failure, list, manyFresh, program, runN, (===))

main ∷ IO ()
main =
  do 
     print "Program terminated normally"

facts ∷ Term → Predicate
facts = \t →
         foldr disconj failure
             [ list [(Atom "voiced"), (Atom "p"), (Atom "b")] === t
             , list [(Atom "voiced"), (Atom "t"), (Atom "d")] === t
             ]

facts3 ∷ Term → Predicate
facts3 = \t ->
          conde
             [ list [(Atom "voiceless"), (Atom "bilabial"), (Atom "p")] === list [(Atom "voiceless"), (Atom "bilabial"), (Atom "p")]
             , list [(Atom "voiced"), (Atom "bilabial"), (Atom "b")] === list [(Atom "voiced"), (Atom "bilabial"), (Atom "b")]
             , list [(Atom "voiceless"), (Atom "alveolar"), (Atom "t")] === list [(Atom "voiceless"), (Atom "alveolar"), (Atom "t")]
             , list [(Atom "voiced"), (Atom "alveolar"), (Atom "d")] === list [(Atom "voiced"), (Atom "alveolar"), (Atom "d")]
             ]

dummy1 :: Term → Predicate
dummy1 ipaAtom = 
   let t = list [(Atom "voiceless"), (Atom "bilabial"), ipaAtom]
   in
                    conde
                         [ list [(Atom "voiceless"), (Atom "bilabial"), (Atom "p")] === t
                         , list [(Atom "voiced"), (Atom "bilabial"), (Atom "b")] === t
                         , list [(Atom "voiceless"), (Atom "alveolar"), (Atom "t")] === t
                         , list [(Atom "voiced"), (Atom "alveolar"), (Atom "d")] === t
                         ]


isVoiceless₂ ipaString = length (runN 1 (\t → dummy1 (Atom ipaString)))  > 0
-- works for "p" and "b" only


dummy₃ :: Term → Predicate
dummy₃ ipaAtom = 
  manyFresh $ \place →
   let t = list [(Atom "voiceless"), place, ipaAtom]
   in
                    conde
                         [ list [(Atom "voiceless"), (Atom "bilabial"), (Atom "p")] === t
                         , list [(Atom "voiced"), (Atom "bilabial"), (Atom "b")] === t
                         , list [(Atom "voiceless"), (Atom "alveolar"), (Atom "t")] === t
                         , list [(Atom "voiced"), (Atom "alveolar"), (Atom "d")] === t
                         ]


isVoiceless₃ ipaString = length (runN 1 (\t → dummy₃ (Atom ipaString)))  > 0

{-- isVoiceless₃ works! See the following:
*InternationalPhoneticAlphabet_ExperimentUsingDSKanren> isVoiceless₃ "p"
True
*InternationalPhoneticAlphabet_ExperimentUsingDSKanren> isVoiceless₃ "b"
False
*InternationalPhoneticAlphabet_ExperimentUsingDSKanren> isVoiceless₃ "d"
False
*InternationalPhoneticAlphabet_ExperimentUsingDSKanren> isVoiceless₃ "t"
True
--}


-- runN 4  (\t → (dummy1 (list [(Atom "voiceless"), (Atom "bilabial"), (Atom "p")] )))


isVoicedO ∷ Term → Predicate
isVoicedO voiced =
      manyFresh $ \place t →
          program ([ list [(Atom "voiced"), place, voiced] === t
                   , facts3 t
                   ])



isVoiced ∷ String → Bool
isVoiced phoneme = not (null kanrenResult)
     where kanrenResult =  runN 1 (\_ → isVoicedO (Atom phoneme))




makeVoicedO ∷ Term → Term → Predicate
makeVoicedO voiceless result =
   conde [ manyFresh $ \place t →
             program ([ list [(Atom "voiceless"), place] === voiceless
                      , list [(Atom "voiced"), place] === result
                      , facts3 t
                      ])]



-- Let us write a wrapper:
makeVoiced ∷ String → String
makeVoiced phoneme = 
     if not (null kanrenResult)
     then 
       case (fst ∘ head) kanrenResult of
              Atom voiced → voiced
              _           → phoneme
     else phoneme  -- do nothing
     where kanrenResult = runN 1 (\t → makeVoicedO (Atom phoneme) t)
           


makeVoicelessO ∷ Term → Term → Predicate
makeVoicelessO voiced result =
  conde [ manyFresh $ \t →
          program ([ list [(Atom "voiced"), result, voiced ] === t
                    , facts t] )]


makeVoiceless ∷ String → String
makeVoiceless phoneme = 
     if not (null kanrenResult)
     then 
       case (fst ∘ head) kanrenResult of
              Atom voiceless → voiceless
              _              → phoneme
     else phoneme  -- do nothing
     where kanrenResult = runN 1 (\ t → makeVoicelessO (Atom phoneme) t)
