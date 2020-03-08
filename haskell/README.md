# phonetics-modeling

The purpose of this project is to model phonetics and phonology in a programming
language according to Linguistics. The source for our information
will be Linguistics textbooks,
and the International Phonetic Alphabet.

Currently, I am using only the textbook with ISBN: 978-1-4051-9103, The
second chapter. It is titled The Sounds of Language.

Currently the program only determines the place, and manner of articulation,
for a handful of consonants (typed in IPA (unicode format)).

Suggested future uses once complete:
Basic Linguistic tasks:
  - determining if a sound has a minimal pair given a list of text.
  - determining if a feature is marked or unmarked in an inventory of sounds
  - converting between IPA and a feature based representation

It is under a permissive license.
Feel free to use it for educational purposes.

This project was started by Elsanussi S. Mneina on July 25, 2019.

## How to execute the code, and write your own.



## How to use
You will need Haskell. I suggest getting the Haskell Platform if you are
new to the language. You will need Haskell Stack.

https://www.haskell.org/platform/



You need to use the terminal, make sure you are in this
directory (where this README file is)  and type

To start an interactive environment where you will be able to execute the
code:

`stack -- ghci`


You can then type in code like the following (starting after the ">", you do not have to type the part before the first ">" symbol on each line.):

`*Main> map analyzeIPA ["ð", "ə"]
`
And you get the following in response.

`[Consonant {vocalFolds = Voiced, place = Dental, manner = Fricative, airstream = PulmonicEgressive},Vowel {height = Mid, backness = Central, rounding = UnmarkedRounding, vocalFolds = Voiced}]`


Then to go the other direction (to IPA symbols):

`*Main> putStrLn $ constructIPA (Consonant Voiceless Alveolar LateralApproximant PulmonicEgressive)`

`l̥`




To go from voiceless to voiced (from IPA symbol to IPA symbol):

`*Main> putStrLn $ voicedIPA "s"`

`z`


`*Main> putStrLn $ devoicedIPA "z"`

`s`

`*Main> putStrLn $ voicedIPA "ʔ"`

`ʔ̼`


`To show the English phoneme inventory:`

`*Main Lib> putStr $ showIPA englishPhonetInventory`

`bpdtgkʔvfðθzsʒʃhd͡ʒt͡ʃmnŋɹ̠jwiuɪʊeoəɛɜʌɔæɐɑɒ`


To show where two consonants differ in their properties (not SPE features, because are not implemented yet):

`unmarkDifferences (Consonant Voiced Alveolar Fricative PulmonicEgressive) (Consonant Voiced Palatal Fricative PulmonicEgressive)`

`Consonant {vocalFolds = Voiced, place = UnmarkedPlace, manner = Fricative, airstream = PulmonicEgressive}`̠


To list all the fricatives in IPA:

Type the following line to define the fricatives:

`*Main Lib> fricatives = generateFromUnmarked (Consonant UnmarkedVocalFolds UnmarkedPlace Fricative PulmonicEgressive)`

Type the following to print out the fricatives together:

`*Main Lib> putStrLn $ concatMap constructIPA fricatives`

`ɸβfvθðszʃʒʂʐçʝxɣχʁħʕhɦʜʢʍʍ̼∅̥∅̼ɕʑ∅̥∅̼`

Type the following to print out the difference in features between two
phonemes:


`*Main English InternationalPhoneticAlphabet Lib PhonemeFeature> featureDifference "z" "s"`


`"[+ voice; ]"`



Note: that the strange zero like symbols at the end represent phones 
that do not have IPA graphemes for the purposes of this program until I can
fix it to provide more useful output. This is not a conventional use of this symbol
for Linguistics!

To build (generates an executable file), use the following command:

`stack build`

To run the executable type:

`stack run`

To run the tests:

`stack test`
