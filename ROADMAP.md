# Roadmap

This text contains plans on what the future of phonetics-modeling project on GitHub will be, but without guarantees.

## 2020-02-25 (February 25th, 2020) 
### Explanation
It is becoming obvious to me that this project needs to be divided into multiple executables. This will make it easier to use one of the functions separately, and greatly increase the maintainability.

I am going to be switching to another programming language for this task. 

I am going to be relying on IPA Numbers (basically integers) declared as constants for the internal tasks of the program, only handling non-ASCII characters at the boundaries. The reason for this is that many diacritic characters simply are illegible and odd to work with in a fixed width font. It is also much easier to remember the IPA numbers, than the unicode escape sequences (although I will rely on constants to make the code more readable).

The functionality will be divided into the following categories:
  - conversion from Unicode characters to IPA Numbers and back.
  - chunking of strings of IPA numbers into groups representing a phoneme.
  - conversion from single-phoneme chunks of IPA Numbers 
    - to vocal quality
    - to place of articulation
    - to secondary articulations
    - to manner of articulation
  - conversion from single-phoneme chunk to SPE features
    - (one for each SPE feature)
  - conversion from a list of SPE features to list of phonemes


