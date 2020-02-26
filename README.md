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


You can then type in code like the following (starting after the ">"):

`*Main> map analyzeIPA ["ð", "ə"]
`
And you get the following in response.

`[Consonant {place = Dental, vocalFolds = Voiced, manner = Fricative, airstream = PulmonicEgressive},Vowel {height = Mid, backness = Central, rounding = Unmarked, vocalFolds = Voiced}]`


Then to go the other direction (to IPA symbols):

`*Main> constructIPA (Consonant  Alveolar  Voiceless LateralApproximant PulmonicEgressive)`

`"l\805"`




To go from voiceless to voiced (from IPA symbol to IPA symbol):

`*Main> voicedIPA "s"`

`"z"`


`*Main> devoicedIPA "z"`

`"s"`


To show the English phoneme inventory:

`*Main Lib> englishPhonetInventory`

`bpdtgkʔvfðθzsʒʃhdʒtʃmnŋɹɹ̠jwiuɪʊeoəɛɜʌɔæɐɑɒ`




To build (generates an executable file), use the following command:

`stack build`

To run the executable type:

`stack run`

To run the tests:

`stack test`
