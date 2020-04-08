module Tester (runTest, printLegend) where

import Prelude (IO, putStrLn, putStr, Bool(True, False), String)
import Prelude.Unicode
  ( (≡), (⧺), (∧), (∨)
  )

conciseMode = True

concisePass = "."
conciseFail = "!"
legend = "\n\nLegend:\n \t" ⧺ concisePass ⧺ "\t passed test\n\t" ⧺ conciseFail ⧺ "\t failed test"


printLegend ∷ IO ()
printLegend = putStrLn legend


runTest ∷ String → Bool → IO ()
runTest description hasPassed =
  if hasPassed
    then putStr (if conciseMode then concisePass else "should be that:" ⧺ description ⧺ "\n")
    else putStrLn (conciseFail  ⧺ "FAILED: " ⧺ description)
