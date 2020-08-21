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

## How to start the server.
Open the terminal or command prompt.

Be in the Haskell directory.
`cd haskell`

Run the following command to start the server.
`stack run phonetics-modeling-server-exe`

Instructions should appear on where to browse to.

## How to start the terminal program.
This program takes text input, and provides 
a text based menu.

Be in the Haskell directory.
`cd haskell`

Run the following command to start the command line
program.

`stack run`


## How to use
You will need Haskell. I suggest getting the Haskell Platform if you are
new to the language. You will need Haskell Stack.

https://www.haskell.org/platform/



You need to use the terminal, make sure you are in this
directory (where this README file is)  and type

To build (generates an executable file), use the following command:

`stack build`

To run the executable type:

`stack run`

## How to run the unit tests.
`stack test`


## How to generate external documentation for source code:

`stack haddock --open phonetics-modeling`

## How to format source code.
Use Stylish-Haskell to ensure that whitespace remains consistent.



## Known Issues
- On Windows 8, the Command Prompt displays non-ASCII characters incorrectly.
