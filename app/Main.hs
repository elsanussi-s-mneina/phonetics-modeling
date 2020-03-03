module Main where
import System.IO (hFlush, stdout)
import Lib
import InternationalPhoneticAlphabet (showIPA, voicedIPA, devoicedIPA)
import English (englishPhonetInventory)

main :: IO ()
main = welcome


welcome :: IO ()
welcome = do
    putStrLn "Please read README.md file for instructions on how to use."
    putStr menu
    hFlush stdout
    selection <- getLine
    putStrLn ("The user selected: " ++ selection ++ "\n")
    putStrLn (case selection of
                             "1" -> showIPA englishPhonetInventory
                             "2" -> voicedIPA "s"
                             "3" -> devoicedIPA "b"
                             otherwise -> "Unrecognized selection. No action taken.")
    putStrLn "\nProgram terminated normally.\n\n"

                      

menu :: String
menu = "What do you want to accomplish?\n\n"
        ++ "1) view the English phoneme inventory (as IPA graphemes).\n"
        ++ "2) make a phoneme voiced.\n"
        ++ "3) make a phoneme unvoiced.\n\n"
        ++ "Enter the number representing your selection below, after the prompt, and press enter/return.\n\n\n(PROMPT:) "