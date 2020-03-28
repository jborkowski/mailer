module ContactFormApp where

import Data.Proxy
import Data.UUID        as U
import Mailer           (Mailer, send)
import Models
import NextID           (NextID)
import NowProvider      (NowProvider)
import Polysemy         (Members, Sem)
import Polysemy.Error
import Polysemy.KVStore (KVStore)
import Servant
import SubscribersList  (subscribe)

type ContactAPI
  = ReqBody '[JSON] ContactForm :> Post '[JSON] ()
  -- :<|> Capture "uid" String :> Get '[JSON] () -- add optout later

api :: Proxy ContactAPI
api = Proxy

server :: Members [KVStore ContactId Contact, Error AppError, NextID, NowProvider, Mailer] r
       => ServerT ContactAPI (Sem r)
server = sendAndSubscribe
  where
    sendNotification = send
    sendAndSubscribe form = do
      sendNotification form
      subscribe form
