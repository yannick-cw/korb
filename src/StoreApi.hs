module StoreApi where

import Cli (WwIdent (WwIdent), ZipCode (ZipCode))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text
import Errors (ApiError, IOE)
import GHC.Generics (Generic)
import HttpClient
import Network.HTTP.Req
import ReweApi.Types (ReweResponse (..))

printValue :: (ToJSON a) => Bool -> a -> IOE e ()
printValue pretty value = liftIO $ BL.putStrLn (encodeIt value)
 where
  encodeIt = if pretty then encodePretty else encode

data PickupMarket = PickupMarket
  { wwIdent :: Text
  , displayName :: Text
  , -- , isPickupStation :: Bool
    city :: Text
  , zipCode :: Text
  , -- , streetWithHouseNumber :: Text
    pickupType :: Text
  }
  deriving stock (Show, Generic, Eq)
newtype Portfolio = Portfolio {pickupMarkets :: [PickupMarket]} deriving stock (Show, Generic)
newtype ServiceArea = ServiceArea {servicePortfolio :: Maybe Portfolio}
  deriving stock (Show, Generic)

instance FromJSON Portfolio
instance ToJSON Portfolio
instance FromJSON PickupMarket
instance ToJSON PickupMarket
instance FromJSON ServiceArea
instance ToJSON ServiceArea
searchForStores :: HttpClient -> ZipCode -> IOE ApiError [PickupMarket]
searchForStores HttpClient{get} (ZipCode plz) = do
  res :: ReweResponse ServiceArea <-
    get (https "mobile-clients-api.rewe.de" /: "api" /: "service-portfolio" /: plz) mempty
  pure $ maybe [] (.pickupMarkets) res.data_.servicePortfolio

storeExists :: HttpClient -> WwIdent -> ZipCode -> IOE ApiError Bool
storeExists client (WwIdent ident) zipCode = do
  stores <- searchForStores client zipCode
  pure $ Prelude.any (\s -> s.wwIdent == ident) stores
