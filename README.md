# phonetics-modeling

The purpose of this project is to model phonetics and phonology in a programming
language according to Linguistics. The source for our information
will be Linguistics textbooks,
and the International Phonetic Alphabet.


## About the International Phonetic Alphabet

IPA stands for international phonetic alphabet. Many parts of this program cannot be understood without reading the IPA chart. The IPA chart is available at  http://www.internationalphoneticassociation.org/content/ipa-chart

and I use the information contained on the IPA chart (specifically the glyphs, and their meainings) under the following license:

"IPA Chart, http://www.internationalphoneticassociation.org/content/ipa-chart, available under a Creative Commons Attribution-Sharealike 3.0 Unported License. Copyright © 2018 International Phonetic Association."

## Textbooks

I am using the textbook with ISBN: 978-1-4051-9103, The
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

### How to run a specific unit test

#### Run all unit tests that have to do with trill
`
stack test --test-arguments='--match "trill"'
`


## How to generate external documentation for source code:

`stack haddock --open phonetics-modeling`


## Known Issues
- On Windows 8, the Command Prompt displays non-ASCII characters incorrectly.

## Previous functionality
In order to focus more on the linguistics part of the programming,
I decided to stop developing a few pieces that were not related.
In the following sections, I will include a link to the source code in the past where the previous
functonality was last available.

### Web Server
- Server functionality has been removed.
You will need to go to an older version of the source code:
- [most recent commit having web server](https://github.com/elsanussi-s-mneina/phonetics-modeling/tree/8ec55d24b800bc1da626082104499e354cbefc75)

### Internationalization
- For internationalization functionality (support for multiple languages in the User interface):
- [most recent commit with internationalization scaffolding](https://github.com/elsanussi-s-mneina/phonetics-modeling/tree/8ec55d24b800bc1da626082104499e354cbefc75)

### Graphical User Interface
- [most recent commit with graphical user interface](https://github.com/elsanussi-s-mneina/phonetics-modeling/tree/434b4b1902791192c059a0be3e077d42c7f6bb80)

