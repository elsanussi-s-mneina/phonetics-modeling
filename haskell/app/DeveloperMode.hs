{-# LANGUAGE UnicodeSyntax #-}

module DeveloperMode (developerMain) where


import Prelude (IO, getLine, putStrLn, return)
import Prelude.Unicode ((⧺), (≡))

import Experimental.CodeGeneration.InternationalPhoneticAlphabetCodeGen (codeGenAllStatementsConstructIPAHaskell)

-- A place where the developer can do those things, that
-- are useful for the developer, but confusing for the user.

-- I created this because I needed a more convenient way to
-- call my experimental modules without having to run
-- the REPL.
developerMain :: IO ()
developerMain =
  do
  putStrLn
        ("This is developer main. Very few come here.\n"
        ⧺ "What do you want to accomplish?\n\n"
        ⧺ "1) Generate Haskell source code for constructIPA, and analyzeIPA functions\n"
        ⧺ "\n"
        )
  userSelection <- getLine 
  if userSelection ≡ "1"
    then putStrLn codeGenAllStatementsConstructIPAHaskell
    else return ()
  return ()

