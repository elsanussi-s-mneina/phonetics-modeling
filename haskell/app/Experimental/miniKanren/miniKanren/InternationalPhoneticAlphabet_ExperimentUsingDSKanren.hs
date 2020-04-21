{- (Experiment ended April 2020)
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

module InternationalPhoneticAlphabet_ExperimentUsingDSKanren (makeVoiced, isVoiced, isVoiceless, isVoicedO, makeVoicedO, makeVoicelessO, makeVoiceless, testString) where 

import Prelude (Bool, String, fst, head, length, not, null, show, (>), ($))
import Prelude.Unicode ((∘), (⧺))
import Language.DSKanren (Term(Atom), Predicate, conde, conj, list, manyFresh, runN, (===))

testString ∷ [String]
testString =
     [ "Demo of miniKanren version of InternationalPhoneticAlphabet module ..."
     , "If one makes /p/ voiced, one gets /" ⧺ makeVoiced "p" ⧺ "/"
     , "If one makes /t/ voiced, one gets /" ⧺ makeVoiced "t" ⧺ "/"
     , "If one makes /b/ voiceless, one gets /" ⧺ makeVoiceless "b" ⧺ "/"
     , "If one makes /d/ voiceless, one gets /" ⧺ makeVoiceless "d" ⧺ "/"
     , "Is /p/ voiced? " ⧺ show (isVoiced "p")
     , "Is /b/ voiced? " ⧺ show (isVoiced "b")
     , "Is /t/ voiceless? " ⧺ show (isVoiceless "t")
     , "Is /d/ voiceless? " ⧺ show (isVoiceless "d")
     , "That is all. Done demonstrating miniKanren version of InternationalPhoneticAlphabet module."
     ]

factsUnifiedWithTerm ∷ Term → [Predicate]
factsUnifiedWithTerm t =
          [ list [(Atom "voiceless"), (Atom "bilabial"), (Atom "p")] === t
          , list [(Atom "voiced"), (Atom "bilabial"), (Atom "b")] === t
          , list [(Atom "voiceless"), (Atom "alveolar"), (Atom "t")] === t
          , list [(Atom "voiced"), (Atom "alveolar"), (Atom "d")] === t
          , list [(Atom "voiceless"), (Atom "glottal"), (Atom "ʔ")] === t
          ]

termVoiceless :: Term → Term
termVoiceless (list [(Atom _), place, grapheme]) = list [(Atom "voiceless"), place, grapheme]

searchThroughVoicedDiacritic t =
  manyFresh $ \grapheme →
     list [(Atom "voiceless"), place, (Atom (grapheme))] === termVoiceless t
     list [(Atom "voiced"), place, (Atom (grapheme:"̬"))] === t

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

isVoiceless :: String → Bool
isVoiceless ipaString = isUnifiable (\_ → isVoicelessO (Atom ipaString))

isVoiced ∷ String → Bool
isVoiced ipaString = isUnifiable (\_ → isVoicedO (Atom ipaString))


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
