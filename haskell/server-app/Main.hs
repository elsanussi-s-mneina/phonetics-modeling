{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Relude

import           UserInterfaceText
                   ( UITextTicket( PleaseReadReadmeMessage
                                 , ProgramTerminatedNormallyMessage
                                 )
                   , NatLanguage(English)
                   , i18n
                   )
import           MainServer (startServer)

-- | This function is where the program starts running.
main :: IO ()
main =
  putTextLn pleaseReadReadmeMessage
  >> startServer
  >> putTextLn programTerminatedNormallyMessage
  where pleaseReadReadmeMessage = i18n English PleaseReadReadmeMessage
        programTerminatedNormallyMessage = i18n English ProgramTerminatedNormallyMessage

