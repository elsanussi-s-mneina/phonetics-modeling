module Main (main) where

import Prelude (IO, putStrLn, Monad((>>)))

import qualified Data.Text as T


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
  putStrLn (T.unpack pleaseReadReadmeMessage)
  >> startServer
  >> putStrLn (T.unpack programTerminatedNormallyMessage)
  where pleaseReadReadmeMessage = i18n English PleaseReadReadmeMessage
        programTerminatedNormallyMessage = i18n English ProgramTerminatedNormallyMessage

