{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           ContactFormApp
import           Control.Monad.Except
import           Data.Function               ((&))
import qualified Data.Text                   as T
import qualified Database.SQLite.Simple      as SQL
import           Lib                         (runKVStoreAsSQLite)
import           Mailer
import           Models                      (AppError (..), Args(..), MailerConfig(..))
import           Network.HaskellNet.Auth     (AuthType (LOGIN), UserName)
import           Network.HaskellNet.SMTP.SSL as SMTP
import qualified Network.Wai.Handler.Warp    as W
import           NextID                      (runNextID)
import           NowProvider                 (runNowProvider)
import           Options.Generic
import           Polysemy                    (Members, Sem)
import qualified Polysemy                    as P
import           Polysemy.Error              (Error (..))
import qualified Polysemy.Error              as PE
import qualified Polysemy.Input              as PI
import           Polysemy.Reader             as PR
import           Servant.Server


createApp :: SQL.Connection -> SMTP.SMTPConnection -> MailerConfig -> IO Application
createApp dbConn smtpConn mailerConfig = return (serve api $ hoistServer api (\sem -> interpret dbConn smtpConn mailerConfig sem) server)
  where
    interpret dbConn smtpConn mailerConfig sem = sem
      & runKVStoreAsSQLite
      & PI.runInputConst dbConn
      & runNextID
      & runNowProvider
      & runMailerOnHaskellNet
      & PI.runInputConst smtpConn
      & PE.runError @AppError
      & PR.runReader mailerConfig
      & P.runM
      & liftToHandler
    liftToHandler = Handler . ExceptT . (fmap handleErrors)
    handleErrors (Left (ReachedDailyLimit id)) = Left err404 {errBody = "Reached daily limit"}
    handleErrors (Right value) = Right value

initDB :: String -> IO SQL.Connection
initDB dbFile = do
  conn <- SQL.open dbFile
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS subscribers (id BLOB PRIMARY KEY, fullName text NOT NULL, emailAddress text NOT NULL, createdAt text NOT NULL, optout text NULL)"
  return conn

main :: IO ()
main = do
  (Args port dbFile smtpUrl smtpUsername smtpPassword mailTo mailName) <- getRecord "Subscriber -- Server"
  smtpConnection <- SMTP.connectSMTPSTARTTLS smtpUrl
  authSucceed <- SMTP.authenticate LOGIN smtpUsername smtpPassword smtpConnection
  if authSucceed then
    print "SMTP authed successfuly"
  else
    print "Authorization Error!"
  dbConnection <- initDB dbFile
  app <- createApp dbConnection smtpConnection (MailerConfig mailTo mailName)
  W.run port app
