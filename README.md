# phonetics-modeling

The purpose of this project is to model phonetics and phonology in a programming
language according to Linguistics. The source for our information
will be Linguistics textbooks,
and the International Phonetic Alphabet.

Currently, I am using only the textbook with ISBN: 978-1-4051-9103, The
second chapter. It is titled The Sounds of Language.

Currently the program does nothing useful.

Suggested future uses once complete:
Basic Linguistic tasks:
  - determining if a sound has a minimal pair given a list of text.
  - determining if a feature is marked or unmarked in an inventory of sounds
  - converting between IPA and a feature based representation

It is under a permissive license.
Feel free to use it for educational purposes.

This project was started by Elsanussi S. Mneina on July 25, 2019.

## How to build / generate executables
You will need Haskell. I suggest getting the Haskell Platform if you are
new to the language. You will need Haskell Stack.

https://www.haskell.org/platform/

To build you need to use the terminal, make sure you are in this
directory (where this README file is)  and type

`stack build`

To run type:

`stack run`

To run the tests:

`stack test`
