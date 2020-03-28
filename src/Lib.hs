{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( runKVStoreAsSQLite
    ) where

import           Database.SQLite.Simple (NamedParam ((:=)))
import qualified Database.SQLite.Simple as SQL
import           Models                 (Contact (..), ContactId (..))
import           Polysemy               (Embed, Member, Members, Sem)
import qualified Polysemy               as P
import qualified Polysemy.Input         as PI
import           Polysemy.KVStore       (KVStore (..))
import qualified Polysemy.State         as PS

runKVStoreAsSQLite :: Member (Embed IO) r
                   => Sem (KVStore ContactId Contact : r) a
                   -> Sem (PI.Input SQL.Connection : r) a
runKVStoreAsSQLite = P.reinterpret $ \case
  UpdateKV id (Just (Contact id_ email fn createdAt optout)) -> do
    conn <- PI.input
    P.embed $ SQL.executeNamed conn
        "INSERT INTO subscribers (id, emailAddress, fullName, createdAt, optout) VALUES (:id, :emailAddress, :fullName, :createdAt, :optout)" 
        [":id" := id, ":emailAddress" := email, ":fullName" := fn, ":createdAt" := createdAt, ":optout" := optout]
