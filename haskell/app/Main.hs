module Main where
import System.IO (hFlush, stdout)
import Lib
import InternationalPhoneticAlphabet (showIPA, voicedIPA, devoicedIPA, describeIPA)
import English (englishPhonetInventory)

main :: IO ()
main = 
  do
    welcome
    putStr menu
    putPrompt
    selection <- getLine
    putStrLn ("The user selected: " ++ selection ++ "\n")
    case selection of
                   "1" -> putStrLn $ showIPA englishPhonetInventory
                   "2" -> promptForPhonemeToVoice
                   "3" -> promptForPhonemeToDevoice
                   "4" -> promptForPhonemeToDescribe
                   otherwise -> putStrLn $ "Unrecognized selection. No action taken."
    putStrLn "\nProgram terminated normally.\n\n"

welcome :: IO ()
welcome = do
    putStrLn "Please read README.md file for instructions on how to use."

promptForPhonemeAndApply :: (String -> String) -> String -> IO ()
promptForPhonemeAndApply func instructions= do
    putStrLn instructions
    putPrompt
    phoneme <- getLine
    putStrLn $ func phoneme

promptForPhonemeToDevoice :: IO ()
promptForPhonemeToDevoice = 
  promptForPhonemeAndApply devoicedIPA "Enter the phoneme you would like to devoice:"

promptForPhonemeToVoice :: IO ()
promptForPhonemeToVoice =  
  promptForPhonemeAndApply voicedIPA "Enter the phoneme you would like to voice:"

promptForPhonemeToDescribe :: IO ()
promptForPhonemeToDescribe =  
  promptForPhonemeAndApply describeIPA "Enter the phoneme you would like to describe:"

putPrompt :: IO ()
putPrompt = do
    putStr prompt
    hFlush stdout

menu :: String
menu = "What do you want to accomplish?\n\n"
        ++ "1) view the English phoneme inventory (as IPA graphemes).\n"
        ++ "2) make a phoneme voiced.\n"
        ++ "3) make a phoneme unvoiced.\n"
        ++ "4) describe a phoneme in English.\n\n"
        ++ "Enter the number representing your selection below, after the prompt, and press enter/return.\n\n\n"

prompt :: String
prompt = "(PROMPT:) "
