{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Mailer (Mailer, runMailerOnHaskellNet, send) where

import           Control.Lens
import qualified Data.ByteString.Char8       as B
import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as TL
import           Models                      (ContactForm (..), email,
                                              firstName, formToAddress,
                                              lastName, simpleAddress,
                                              MailerConfig(..))
import           Network.HaskellNet.Auth     (AuthType (LOGIN), UserName)
import           Network.HaskellNet.SMTP.SSL as SMTP
import           Network.Mail.Mime           (Address (..), Mail (..),
                                              addressEmail, addressName,
                                              htmlPart, mailBcc, mailCc,
                                              mailFrom, mailHeaders, mailParts,
                                              mailTo, simpleMail)
import           Polysemy                    (Embed, Members, Sem)
import           Polysemy.Reader             as PR  
import qualified Polysemy                    as P
import qualified Polysemy.Input              as PI

data Mailer m a where
  Send :: ContactForm -> Mailer m ()

P.makeSem ''Mailer

runMailerOnHaskellNet :: Members [Embed IO, PR.Reader MailerConfig] r
                      => Sem (Mailer : r) a
                      -> Sem (PI.Input SMTP.SMTPConnection : r) a
runMailerOnHaskellNet = P.reinterpret $ \case
  Send form -> do
    conn <- PI.input
    config <- PR.ask
    P.embed $ sendEmail conn config form

sendEmail :: SMTP.SMTPConnection -> MailerConfig -> ContactForm -> IO ()
sendEmail smtpConn (MailerConfig to name) form =
  SMTP.sendMimeMail2 mail smtpConn
    where
      subject = "Wypełniono nowy fromularz kontaktowy!"
      mailFrom = view formToAddress form
      mailTo = simpleAddress name to
      html = TL.pack . contactMessage $ form
      mail = Mail { mailFrom    = mailTo
                      , mailTo      = [mailTo]
                      , mailCc      = []
                      , mailBcc     = []
                      , mailHeaders = [("Reply-To", addressEmail mailFrom), ("Subject", subject)]
                      , mailParts   = [[htmlPart html]]
                      }

contactMessage :: ContactForm -> String
contactMessage (ContactForm fn ln _ phone age diagnosis comment experience _ _) =
  "<h3>Dane kontaktowe:</h3>" <>
  "<ul>" <>
  "<li>Imię: " <> show fn <> "</li>" <>
  "<li>Nazwisko: " <> show ln <> "</li>" <>
  "<li>Numer kontaktowy: " <> show phone <> "</li>" <>
  "<li>Wiek: " <> show age <> "</li>" <>
  "<li>Diagnoza: " <> show diagnosis <> "</li>" <>
  "<li>Komentarz: " <> show comment <> "</li>" <>
  "<li>Doświadczenie z bioenergoterapią: " <> show experience <>"</li>" <>
  "</ul>"
