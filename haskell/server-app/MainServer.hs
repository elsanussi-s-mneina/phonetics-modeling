{-
This module allows certain functionality of the program to be used
over the internet or a local area network.

It sets up a server over HTTP locally.

The API follows REST conventions.
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
module MainServer where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import           Relude

import EnglishUSText (beforeServerStartMessage)
import  IPA          (devoicedIPA, voicedIPA, describeIPA)

type API = "voice_phoneme" :> Capture "phoneme" Text  :> Get '[JSON] Text
      :<|> "devoice_phoneme" :> Capture "phoneme" Text  :> Get '[JSON] Text
      :<|> "describe_phoneme" :> Capture "phoneme" Text :> Get '[JSON] Text

api :: Proxy API
api = Proxy

server :: Server API
server = voicePhonemeWrapper :<|> devoicePhonemeWrapper
       :<|> describePhonemeWrapper

voicePhonemeWrapper :: Text -> Handler Text
voicePhonemeWrapper phoneme = return (voicedIPA phoneme)

devoicePhonemeWrapper :: Text -> Handler Text
devoicePhonemeWrapper phoneme = return (devoicedIPA phoneme)

describePhonemeWrapper :: Text -> Handler Text
describePhonemeWrapper phoneme = return (describeIPA phoneme)

app :: Application
app = serve api server


startServer :: IO ()
startServer = do
  putTextLn beforeServerStartMessage
  run 8080 app