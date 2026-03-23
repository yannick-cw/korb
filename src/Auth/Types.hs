module Auth.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Errors (AppError, AuthError, IOE)
import GHC.Generics

data TokenStore = TokenStore
  { readAccess :: IOE AuthError AccessToken
  , readRefresh :: IOE AuthError RefreshToken
  , storeAccess :: AccessToken -> IOE AuthError ()
  , storeRefresh :: RefreshToken -> IOE AuthError ()
  }

data Auth = Auth
  { getValidToken :: IOE AppError AccessToken
  , login :: IOE AppError Text
  }

newtype PKCEVerifier = PKCEVerifier ByteString

newtype AuthCode = AuthCode Text deriving (Show)
newtype AccessToken = AccessToken Text deriving (Show, Generic)
instance FromJSON AccessToken
instance ToJSON AccessToken
newtype RefreshToken = RefreshToken Text deriving (Show, Generic)
instance FromJSON RefreshToken
instance ToJSON RefreshToken
data TokenResponse = TokenResponse
  { access_token :: AccessToken
  , expires_in :: Int
  , refresh_expires_in :: Int
  , refresh_token :: RefreshToken
  , token_type :: Text
  , id_token :: Text
  , scope :: Text
  }
  deriving stock (Show, Generic)
instance FromJSON TokenResponse
instance ToJSON TokenResponse

newtype Exp = Exp {exp :: Int64} deriving (Generic, Show)
instance FromJSON Exp
