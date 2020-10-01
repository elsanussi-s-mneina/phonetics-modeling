In this folder we put
lists of phonemes for 
a particular language.

The correct terminology for this
in Linguistics is a "phoneme inventory."

Unfortunately, we are forced to be more exact
in our specification of the phonemes than a linguist
would be, due to the way I defined the types. We
could fix this by defining a phoneme range type,
or other more complex types, but it is not worth
the effort at this point (as of
September 30th, 2020). Perhaps, in the future
it might be.

The main use of these phoneme inventories currently
is to act as a quick reference for users of the program.

It also helps the programmer find errors in the program,
and shows the programmer which phonemes the program fails to support.

## Notes about Language Variation
It is not easy to cover the sound inventory of a language in
a way that covers all its variations.

I was considering having different sound inventories for
different variations of a language, and a way of combining
them, but this is not implemented yet.

The biggest difficulty is that we have no way of
representing allophones right now, which is a really
important concept in linguistics.
