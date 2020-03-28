{-# LANGUAGE TemplateHaskell #-}
module NextID (next, runNextID, NextID) where

import           Data.UUID.V4 as UUID
import           Models       (ContactId (..))
import           Polysemy     (Embed, Member, Sem)
import qualified Polysemy     as P

data NextID m a where
  Next :: NextID m ContactId

P.makeSem ''NextID

runNextID :: Member (Embed IO) r
          => Sem (NextID ': r) a
          -> Sem r a
runNextID = P.interpret $ \case
  Next -> P.embed $ ContactId <$> UUID.nextRandom
