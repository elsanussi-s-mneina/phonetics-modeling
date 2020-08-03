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

`stack -- ghci --ghci-options -XOverloadedStrings`


You can then type in code like the following (starting after the ">", you do not have to type the part before the first ">" symbol on each line.):

`*Main> doAnalyzeIPA "ð"
`
And you get the following in response.

`"voiced dental fricative pulmonic egressive consonant"`


Then to go the other direction (to IPA symbols):

`*Main> doConstructIPA (Consonant Voiceless Alveolar LateralApproximant PulmonicEgressive)`

`l̥`




To go from voiceless to voiced (from IPA symbol to IPA symbol):

`*Main> putTextLn $ voicedIPA "s"`

`z`


`*Main> putTextLn $ devoicedIPA "z"`

`s`

`*Main> putTextLn $ voicedIPA "ʔ"`

`ʔ̼`


`To show the English phoneme inventory:`

`*Main Lib> putTextLn $ showIPA englishPhonetInventory`

`bpdtgkʔvfðθzsʒʃhd͡ʒt͡ʃmnŋɹ̠jwiuɪʊeoəɛɜʌɔæɐɑɒ`



Note: that the strange zero like symbols (Actually the symbol for the empty set) (∅) at the end represent phones 
that do not have IPA graphemes for the purposes of this program until I can
fix it to provide more useful output. This is not a conventional use of this symbol
for Linguistics!

To build (generates an executable file), use the following command:

`stack build`

To run the executable type:

`stack run`


## How to compile the code for the terminal

In the command line enter:

`stack build --pedantic`

## How to compile the code for the GUI
In the command line enter:

`stack install --flag fltkhs:bundled`

This package uses the Fltkhs package for
making windows and other graphical user
interface widgets. For information
on Fltkhs see: https://github.com/deech/fltkhs

So far no windows have been run, but the 
project is ready to compile with Fltkhs
dependencies.

## How to run the unit tests.
`stack test`


## How to generate documentation for source code:

`stack haddock --open phonetics-modeling`

## How to format source code.
Use Stylish-Haskell to ensure that whitespace remains consistent.



## Known Issues
- On Windows 8, the Command Prompt incorrectly displays non-ASCII characters.
