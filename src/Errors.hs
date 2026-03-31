module Errors (
  ApiError (..),
  AuthError (..),
  FileError (..),
  AppError (..),
  IOE,
  LiftError (..),
  liftIOE,
) where

import Control.Exception (IOException, try)
import Control.Monad.Trans.Except (ExceptT (..), withExceptT)
import Data.Bifunctor (first)
import Data.Text (Text, pack, unpack)

data ApiError = ApiError Text | DecodingError Text deriving stock (Eq)
instance Show ApiError where
  show (ApiError t) = "APIError: " <> unpack t
  show (DecodingError t) = "Decoding error: " <> unpack t
newtype AuthError = AuthError Text deriving newtype (Show)
data FileError = FileError Text | FileReadError Text | TokenReadError Text
  deriving stock (Show)
data AppError
  = AppApiError ApiError
  | AppFileError FileError
  | AppAuthError AuthError

instance Show AppError where
  show (AppApiError e) = show e
  show (AppFileError e) = show e
  show (AppAuthError e) = show e

liftIOE :: (Text -> e) -> IO a -> IOE e a
liftIOE mkError action = ExceptT $ first (mkError . pack . show) <$> try @IOException action

type IOE e a = ExceptT e IO a

class LiftError e where
  liftE :: IOE e a -> IOE AppError a

instance LiftError ApiError where liftE = withExceptT AppApiError
instance LiftError AuthError where liftE = withExceptT AppAuthError
instance LiftError FileError where liftE = withExceptT AppFileError
instance LiftError AppError where liftE = id
