{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import           Relude

import           UserInterfaceText
                   ( UITextTicket(
                                  ProgramTerminatedNormallyMessage
                                 )
                   , NatLanguage(English)
                   , i18n)
import           MainWindow (openWindow)

-- | This function is where the program starts running.
main :: IO ()
main =
  openWindow
  >> putTextLn (i18n English ProgramTerminatedNormallyMessage)