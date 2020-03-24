{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Models where

import GHC.Generics
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)

type Name = String
type Email = String
type PhoneNumber = String

data ContactForm = ContactForm { _firstName    :: Name
                               , _lastName     :: Name
                               , _email        :: Email
                               , _phoneNumber  :: PhoneNumber
                               , _age          :: Int
                               , _diagnosis    :: String
                               , _comments     :: Maybe String
                               , _experiance   :: Maybe String
                               , _aboutTherapy :: Bool
                               , _acceptTerms  :: Bool
                               } deriving (Show, Eq, Generic)

deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''ContactForm




data AppError = ReachedDailyLimit Email
