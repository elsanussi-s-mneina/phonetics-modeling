{-
In this file, I show my experiments using MiniKanren, a relational logic language.
Specifically I use the ds-karen implementation of it as a domain specific language hosted in Haskell.

The reason I am doing this is that, there have been issues with maintainability of the 
code in the InternationalPhoneticAlphabet module. Firstly, it is twice as large as it needs to be,
because for some functions there we have to define them, and their inverse (specifically constructIPA,
and analyzeIPA). But the code there is really repetitive.

But, the attempts I made at making the code shorter added so many conditions, 
and made the logic very complicated, with many nested conditions. 
We even did something where we used indices
into a two dimensional "array" (really a list of lists). And I took out that because
the code was far more convoluted than it needed to be. This became apparent to me,
when I translated the code to Elm, and Rust.

The other problem I had is that to handle the special diacritics that can make consonants
voiced or voiceless, I used recursion, but I had to split a function into multiple functions
to avoid infinite recursion. For example: instead of just having the reasonable one function
constructIPA, I had constructIPA calling constructIPA1, calling constructIPA2, and some recursion.
In any case, it worked at ensuring the call to constructIPA (or analyzeIPA) always terminated, 
but it was hard to follow.

Therefore, I decided to try using MiniKanren for this task. MiniKanren has a few features that 
make it suitable specifically for this problem. The first is that it uses unification, and the
second is that you can limit the number of results it tries to return.


As of April 7, 2020, I was able to get meaningful results from short examples that I made.

Note for future reference: the example using appendo that came with the library 
as a comment in one of the modules did not
work for me. I came up with my own examples below which were more relevant for my use case.


Conclusions for April 7, 2020:
  - minikanren seems promising, but we should try to find a Prolog DSL, 
    because it would be easier to understand.
  - I should try a few other libraries for minikanren, before settling



You need the following dependency to follow along in the examples:

"ds-kanren-0.2.0.1@sha256:b4275541bcc48385d04f8c225509359191a2e0d5faaa078f1cc4e2c52db3d1d8,1527"


I used the command 
stack -- ghci
in the "phonetics-modeling/haskell" directory

(Note: "*..>" is notation here for the prompt in GHCi)
Then run:
*..> :l Experimental/miniKanren/BeginningExperimentUsingDSKanren.hs 

Then run main:
*..> main

to check that the code runs.
-}
{-# LANGUAGE UnicodeSyntax #-}
module BeginningExperimentUsingDSKanren where 

import Language.DSKanren
import Prelude.Unicode ((∘))


main =
  do 
     print $ map fst ∘ runN 1 $ (\t → success)
     -- Expected result:
     -- [_0]


     -- Create my first real predicate.
     print $ map fst ∘ runN 1 $ (\t → Atom "3" === Atom "4")
     -- Expected result:
     -- []
     print $ map fst ∘ runN 1 $ (\t → Atom "3" === Atom "3")
     -- Expected result:
     -- [_0]
     print $ map fst ∘ runN 1 $ (\t → Atom "p" === Atom "b")
     -- Expected result:
     -- []
     print $ map fst ∘ runN 1 $ (\t → list [Atom "voiced", Atom "p"] === Atom "b")
     -- Expected result:
     -- []



     -- Using disconjunction:
     print $ map fst ∘ runN 1 $ (\t → disconj ( Atom "a" === Atom "a") (Atom "b" === Atom "c"))
     -- Expected result:
     -- [_0]

     print $ map fst ∘ runN 1 $ (\t → disconj ( Atom "a" === Atom "q") (Atom "b" === Atom "c"))
     -- Expected result:
     -- []



     -- Using unification:
     print $ map fst ∘ runN 1 $ (\t → disconj ( Atom "a" === Atom "q") (Atom "b" === t))
     -- Expected result:
     -- ['b]

     print $ map fst ∘ runN 1 $ (\t → disconj ( t === Atom "q") (Atom "b" === t))
     -- Expected result:
     -- ['q]


     -- Using conde:
     -- It is sort of like "and"
     print $ runN 3 $ (\t → conde [(t === Atom "c"), (t === Pair (Atom "a") ( Atom "b"))])
     -- Expected result:
     -- [('c,[]),(('a, 'b),[])]



     -- Trying to define facts:


     -- Let's try to make something useful
     print $ runN 3 $ (\t → conde [(Pair (Atom "voiced") (Atom "p") === Atom "b"), (Pair (Atom "voiced") (Atom "t") === Atom "d")])
     -- []

-- okay now let us allow querying:
factsAboutVoicing = (\t → conde [ 
                              list [(Atom "voiced"), (Atom "p"), (Atom "b")] === t, 
                              list [(Atom "voiced"), (Atom "t"), (Atom "d")] === t])


demoFactsAboutVoicing =
     print $ runN 3 $ factsAboutVoicing
     -- [(('voiced, ('p, ('b, 'nil))),[]),(('voiced, ('t, ('d, 'nil))),[])]

-- Alright better, than nothing, but still not useful.
-- it only returns all the facts, it does not let us query any more specifically.
-- I used a strange trick I thought up, to use the t variable as true.


factsAboutVoicingStrong = \t →
         foldr disconj failure
             [ list [(Atom "voiced"), (Atom "p"), (Atom "b")] === t
             , list [(Atom "voiced"), (Atom "t"), (Atom "d")] === t
             ]

isVoicedO ∷ Term → Predicate
isVoicedO voiced =
  conde [ manyFresh $ \t voiceless →
          program ([ list [(Atom "voiced"), voiceless, voiced] === t
                    , factsAboutVoicingStrong t] )]


isVoicedODemo =
     do
     print $ runN 3 $ (\t → isVoicedO (Atom "p"))
     -- []
     -- That means it is not voiced, which is correct.


     print $ runN 3 $ (\t → isVoicedO (Atom "b"))
     -- [(_0,[])]
     -- That means it is voiced. Finally we succeeded. So far.

-- Now let us make a wrapper function:

isVoiced ∷ String → Bool
isVoiced phoneme = not (null kanrenResult)
     where kanrenResult =  runN 1 (\_ → isVoicedO (Atom phoneme))


isVoicedDemo ∷ IO ()
isVoicedDemo =
     do
     print $ isVoiced "p"
     -- False
     print $ isVoiced "b"
     -- True
     print $ isVoiced "t"
     -- False
     print $ isVoiced "d"
     -- True

-- It works.


-- Okay, now we just need a way to query the voiced counterpart when we give the voiceless one.
-- 


makeVoicedO ∷ Term → Term → Predicate
makeVoicedO voiceless result =
   conde [ manyFresh $ \ t →
             program ([ list [(Atom "voiced"), voiceless, result] === t
                      , factsAboutVoicingStrong t] )]



-- It works!

makeVoicedODemo = 
     do
     print $ runN 3 $ (\ t → makeVoicedO (Atom "p") t)
     -- [('b,[])]
     print $ runN 3 $ (\ t → makeVoicedO (Atom "b") t)
     -- []
     print $ runN 3 $ (\ t → makeVoicedO (Atom "s") t)
     -- []
     print $ runN 3 $ (\ t → makeVoicedO (Atom "t") t)
     -- [('d,[])]


-- Let us write a wrapper:
makeVoiced ∷ String → String
makeVoiced phoneme = 
     if not (null kanrenResult)
     then let Atom voiced = (fst ∘ head) kanrenResult
          in voiced
     else phoneme  -- do nothing
     where kanrenResult = runN 1 (\t → makeVoicedO (Atom phoneme) t)
           


makeVoicelessO ∷ Term → Term → Predicate
makeVoicelessO voiced result =
  conde [ manyFresh $ \t →
          program ([ list [(Atom "voiced"), result, voiced] === t
                    , factsAboutVoicingStrong t] )]


makeVoiceless ∷ String → String
makeVoiceless phoneme = 
     if not (null kanrenResult)
     then let Atom voiced = (fst ∘ head) kanrenResult
          in voiced
     else phoneme  -- do nothing
     where kanrenResult = runN 1 (\t → makeVoicelessO (Atom phoneme) t)


makeVoicelessDemo =
     do 
     print $ makeVoiceless "d"
     -- "t"
     print $ makeVoiceless "b"
     -- "p"
     print $ makeVoiceless "p"
     -- "p"
     print $ makeVoiceless "t"
     -- "t"
     print $ makeVoiceless "q"
     -- "q"
-- It works.