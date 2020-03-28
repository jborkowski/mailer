{-# LANGUAGE TemplateHaskell #-}
module NowProvider (now, NowProvider, runNowProvider) where

import           Data.Time.Clock  (getCurrentTime, UTCTime)
import           Polysemy (Member, Embed, Sem)
import qualified Polysemy as P

data NowProvider m a where
  Now :: NowProvider m UTCTime

P.makeSem ''NowProvider

runNowProvider :: Member (Embed IO) r
               => Sem(NowProvider ': r) a
               -> Sem r a
runNowProvider = P.interpret $ \case
  Now -> P.embed getCurrentTime 