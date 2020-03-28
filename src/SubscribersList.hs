module SubscribersList where

import           Control.Lens
import qualified Data.UUID              as U
import           Polysemy               as P
import           Polysemy               (Member, Members, Sem)
import qualified Polysemy.Input         as PI
import           Polysemy.State

import           Mailer                 (Mailer, send)
import           NextID
import           Polysemy.KVStore       (KVStore)
import qualified Polysemy.KVStore       as Store

import qualified Database.SQLite.Simple as SQL
import           Models                 (Contact (..), ContactForm (..),
                                         ContactId (..), acceptTerms, email,
                                         fullName)
import           NowProvider            (NowProvider, now)

subscribe :: Members [KVStore ContactId Contact, NextID, NowProvider] r => ContactForm -> Sem r ()
subscribe form = do
  id <- next
  createdAt <- now
  Store.writeKV id (contact id createdAt)
  where
    contact id createdAt = Contact { _id = id
                                   , _emailAddress = view email form
                                   , _fullName     = view fullName form
                                   , _createdAt    = createdAt
                                   , _optouted     = Nothing
                                   }
