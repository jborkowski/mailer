{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Models where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
                                                 --  fieldLabelModifier)
import qualified Data.ByteString.Lazy             as LB
import           Data.Functor                     ((<$>))
import           Data.String                      (IsString)
import qualified Data.Text                        as T
import           Data.Time.Clock                  (UTCTime)
import qualified Data.UUID                        as U
import           Database.SQLite.Simple           (FromRow, SQLData (SQLBlob),
                                                   ToRow, field, fromRow, toRow)
import           Database.SQLite.Simple.FromField (FromField, ResultError (..),
                                                   fromField, returnError)
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.Ok
import           Database.SQLite.Simple.ToField   (ToField, toField)
import           Database.SQLite.Simple.Types
import           GHC.Generics
import           Network.Mail.Mime                (Address (..))
import           Options.Generic

newtype Name = Name T.Text deriving (IsString, Show)
newtype Email = Email T.Text deriving (IsString, Show)
newtype PhoneNumber = PhoneNumber T.Text deriving (IsString, Show)

data ContactForm = ContactForm { _firstName    :: T.Text
                               , _lastName     :: T.Text
                               , _email        :: T.Text
                               , _phoneNumber  :: T.Text
                               , _age          :: Int
                               , _diagnosis    :: T.Text
                               , _comments     :: Maybe T.Text
                               , _experiance   :: Maybe T.Text
                               , _aboutTherapy :: Bool
                               , _acceptTerms  :: Bool
                               } deriving (Show, Generic)

 -- curl -X POST -d '{"firstName": "Lucyna", "lastName": "Borkowska", "email":"lubo@pm.me", "phoneNumber":"542319882", "age":12,"diagnosis":"Some Diag", "aboutTherapy": true, "acceptTerms": true}' -H 'Content-type: application/json' http://localhost:9000

customOptions = defaultOptions
                { fieldLabelModifier = drop 1
                }

instance FromJSON ContactForm where
    parseJSON = genericParseJSON customOptions

newtype ContactId = ContactId U.UUID deriving (Eq, Ord, Show)

instance ToField ContactId where
  toField (ContactId uuid) = toField . U.toASCIIBytes $ uuid

instance FromField ContactId where
  fromField f@(Field (SQLBlob blb) _) =
    case U.fromLazyASCIIBytes . LB.fromChunks $ [blb] of
      Just uuid -> Ok . ContactId $ uuid
      Nothing -> returnError ConversionFailed f "cannot parse Lazy ByteString from chunks"
  fromField f                       = returnError ConversionFailed f "expecting SQLBlob column type"

makeLenses ''ContactForm

data Contact = Contact { _id           :: ContactId
                       , _emailAddress :: T.Text
                       , _fullName     :: T.Text
                       , _createdAt    :: UTCTime
                       , _optouted     :: Maybe UTCTime
                       } deriving (Show)

instance FromRow Contact where
  fromRow = Contact <$> field <*> field <*> field <*> field <*> field

instance ToRow Contact where
  toRow (Contact id_ email fn createdAt optouted) = toRow (id_, email, fn, createdAt, optouted)

data AppError = ReachedDailyLimit Email

receipt :: Lens' ContactForm T.Text
receipt = lens getter setter
  where
    getter form = view firstName form <> " " <> view lastName form <> " <" <> view email form <> ">"
    setter form new = undefined

fullName :: Lens' ContactForm T.Text
fullName = lens getter setter
  where
    getter user = view firstName user <> " " <> view lastName user
    setter user new =
      let withFirstName = set firstName (head (T.words new)) user
      in set lastName (T.unwords . tail . T.words $ new) withFirstName

formToAddress :: Lens' ContactForm Address
formToAddress = lens getter setter
  where
    getter form = simpleAddress (Just (view firstName form <> " " <> view lastName form)) (view email form)
    setter = undefined

simpleAddress :: (Maybe T.Text) -> T.Text -> Address
simpleAddress name email = Address { addressName = name
                                   , addressEmail = email
                                   }


data Args = Args { port         :: Int
                 , dbFile       :: String
                 , smtpUrl      :: String
                 , smtpUsername :: String
                 , smtpPassword :: String
                 , mailTo       :: T.Text 
                 , mailName     :: Maybe T.Text 
                } deriving (Show, Generic)
instance ParseRecord Args

data MailerConfig = MailerConfig { to :: T.Text,  name :: Maybe T.Text } deriving Show