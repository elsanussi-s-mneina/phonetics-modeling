# Changelog for phonetics-modeling

## Unreleased changes

April 26, 2020:
Switched from Prelude to Relude which is a replacement for Prelude.

- The biggest change is that we now use <> instead of ++, and
    have to treat lists differently because we use the NonEmpty type.

- I also updated the LTS version in order to use the latest features in Relude.

