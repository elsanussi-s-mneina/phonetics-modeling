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

module InternationalPhoneticAlphabet_ExperimentUsingDSKanren (makeVoiced, isVoiced, main, isVoicedO, makeVoicedO, makeVoicelessO, makeVoiceless) where 

import Prelude (Bool, IO, String, foldr, fst, head, length, not, null, print, (>), ($))
import Prelude.Unicode ((∘), (⧺))
import Language.DSKanren (Term(Atom), Predicate, conde, conj, disconj, failure, list, manyFresh, program, runN, (===))

main ∷ IO ()
main =
  do 
     print "Program terminated normally"

factsUnifiedWithTerm ∷ Term → [Predicate]
factsUnifiedWithTerm t =
          [ list [(Atom "voiceless"), (Atom "bilabial"), (Atom "p")] === t
          , list [(Atom "voiced"), (Atom "bilabial"), (Atom "b")] === t
          , list [(Atom "voiceless"), (Atom "alveolar"), (Atom "t")] === t
          , list [(Atom "voiced"), (Atom "alveolar"), (Atom "d")] === t
          ]


isVocalFoldStateO :: String → Term → Predicate
isVocalFoldStateO vocalFoldState ipaAtom = 
  manyFresh $ \place →
   let t = list [(Atom vocalFoldState), place, ipaAtom]
   in
      conde
         (factsUnifiedWithTerm t)  


isVoicelessO :: Term → Predicate
isVoicelessO = isVocalFoldStateO "voiceless"

isVoicedO :: Term → Predicate
isVoicedO = isVocalFoldStateO "voiced"

isUnifiable termToPredicate =  length (runN 1 termToPredicate)  > 0

isVoiceless ipaString = isUnifiable (\t → isVoicelessO (Atom ipaString))
isVoiced ipaString = isUnifiable (\t → isVoicedO (Atom ipaString))


makeVoicedO ∷ Term → Term → Predicate
makeVoicedO voiceless result =
   conde [ manyFresh $ \place →
             let t = list [(Atom "voiceless"), place, voiceless]
                 t₂ = list [(Atom "voiced"), place, result]
               in conj 
                    (conde (factsUnifiedWithTerm t))
                    (conde (factsUnifiedWithTerm t₂)) 
         ]


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
   conde [ manyFresh $ \place →
             let t = list [(Atom "voiceless"), place, result]
                 t₂ = list [(Atom "voiced"), place, voiced]
               in conj 
                    (conde (factsUnifiedWithTerm t))
                    (conde (factsUnifiedWithTerm t₂)) 
         ]



makeVoiceless ∷ String → String
makeVoiceless phoneme = 
     if not (null kanrenResult)
     then 
       case (fst ∘ head) kanrenResult of
              Atom voiceless → voiceless
              _              → phoneme
     else phoneme  -- do nothing
     where kanrenResult = runN 1 (\ t → makeVoicelessO (Atom phoneme) t)
