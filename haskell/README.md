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


## How to compile the code for the terminal

In the command line enter:

`stack build --pedantic`

## How to compile the code for the GUI
(Note I could not get the GUI to compile on Windows.
I was able to get it to work on Mac OS Mojave)

Delete the current phonetics-modeling.cabal file.

`rm phonetics-modeling.cabal`

Rename the cabal file for GUI to replace the deleted file.

`mv phonetics-modeling.cabal.forGUI`

In the command line enter:

`stack install --flag fltkhs:bundled`

This package uses the Fltkhs package for
making windows and other graphical user
interface widgets. For information
on Fltkhs see: https://github.com/deech/fltkhs

So far no windows have been created, but the 
project is ready to compile with Fltkhs
dependencies.

Run stack build, and stack run as usual, and a window will appear.

`stack build`

`stack run`


When you are done with the GUI project,
simply rename it back to how it was. Use
the following command:

`mv phonetics-modeling.cabal phonetics-modeling.cabal.forGUI`

## How to run the unit tests.
`stack test`


## How to generate external documentation for source code:

`stack haddock --open phonetics-modeling`

## How to format source code.
Use Stylish-Haskell to ensure that whitespace remains consistent.



## Known Issues
- On Windows 8, the Command Prompt displays non-ASCII characters incorrectly.
