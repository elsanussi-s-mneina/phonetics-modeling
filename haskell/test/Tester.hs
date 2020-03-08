module Tester (runTest, printLegend) where

import Prelude ((++), IO, putStrLn, putStr, (==), Bool(True, False), String)


conciseMode = True

startRed = "\x1b[31m"

startGreen = "\x1b[32m"
endColor = "\x1b[0m" 
concisePass = startGreen ++ "." ++ endColor
conciseFail = startRed ++ "!" ++ endColor
legend = "\n\nLegend:\n \t" ++ concisePass ++ "\t passed test\n\t" ++ conciseFail ++ "\t failed test"


printLegend :: IO ()
printLegend = putStrLn legend


runTest :: String -> Bool -> IO ()
runTest description hasPassed = do
  if hasPassed
    then putStr (if conciseMode then "\x1b[32m.\x1b[0m" else ("\x1b[32m should be that: \x1b[0m" ++ description ++ "\n"))
    else putStrLn ("! \x1b[35m SHOULD \x1b[31mBE THAT: \x1b[0m" ++ description ++ " FAILED!!!")
