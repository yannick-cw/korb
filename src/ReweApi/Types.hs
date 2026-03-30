module ReweApi.Types where

import Data.Aeson (
  FromJSON (parseJSON),
  Options (fieldLabelModifier),
  ToJSON (toJSON),
  Value (String),
  defaultOptions,
  genericParseJSON,
 )
import Data.Text (Text, pack, toLower)
import Data.Time (ZonedTime)
import GHC.Generics (Generic)

-- Shared primitives

newtype CentPrice = CentPrice Int deriving stock (Generic, Show, Eq)
instance FromJSON CentPrice
instance ToJSON CentPrice

newtype ListingId = ListingId Text deriving stock (Generic, Show, Eq)
instance FromJSON ListingId
instance ToJSON ListingId

newtype ProductId = ProductId Text deriving stock (Generic, Show, Eq)
instance FromJSON ProductId
instance ToJSON ProductId

newtype ItemId = ItemId Text deriving stock (Generic, Show, Eq)
instance FromJSON ItemId
instance ToJSON ItemId

-- Product (shared across search, favorites, basket line items)

data ProductAttributes = ProductAttributes
  { isOrganic :: Maybe Bool
  , isRegional :: Maybe Bool
  }
  deriving stock (Generic, Show, Eq)
instance FromJSON ProductAttributes
instance ToJSON ProductAttributes

data Listing = Listing
  { listingId :: ListingId
  , currentRetailPrice :: CentPrice
  , grammage :: Maybe Text
  }
  deriving stock (Generic, Show, Eq)
instance ToJSON Listing
instance FromJSON Listing

data Product = Product
  { articleId :: Text
  , productId :: ProductId
  , title :: Text
  , imageURL :: Text
  , orderLimit :: Maybe Int
  , listing :: Listing
  , attributes :: Maybe ProductAttributes
  , itemId :: Maybe ItemId
  }
  deriving stock (Generic, Show, Eq)
instance ToJSON Product
instance FromJSON Product

-- Search (GET /products)

data SearchAttribute = Organic | Regional | Vegan | Vegetarian deriving stock (Generic, Show, Eq)
attributeToText :: SearchAttribute -> Text
attributeToText = toLower . pack . show

newtype SearchProducts = SearchProducts {products :: [Product]} deriving stock (Generic, Show, Eq)
instance ToJSON SearchProducts
instance FromJSON SearchProducts

newtype SearchResponse = SearchResponse {products :: SearchProducts}
  deriving stock (Generic, Show, Eq)
instance ToJSON SearchResponse
instance FromJSON SearchResponse

-- Favorites (GET /favorites, POST /favorites/{listId}/lineitems, DELETE /favorites/{listId}/lineitems/{itemId})

newtype FavoriteListId = FavoriteListId Text deriving stock (Generic, Show, Eq)
instance FromJSON FavoriteListId
instance ToJSON FavoriteListId

data FavoriteList = FavoriteList
  { id :: FavoriteListId
  , name :: Text
  , items :: [Product]
  }
  deriving stock (Generic, Show, Eq)
instance FromJSON FavoriteList
instance ToJSON FavoriteList

newtype FavoriteLists = FavoriteLists {favorites :: [FavoriteList]}
  deriving stock (Generic, Show, Eq)
instance FromJSON FavoriteLists
instance ToJSON FavoriteLists

newtype FavoritesResponse = FavoritesResponse {favoriteLists :: FavoriteLists}
  deriving stock (Generic, Show, Eq)
instance FromJSON FavoritesResponse
instance ToJSON FavoritesResponse

data AddFavoriteReq = AddFavoriteReq
  { listingId :: ListingId
  , quantity :: Maybe Int
  , productId :: ProductId
  }
  deriving stock (Show, Generic)
instance ToJSON AddFavoriteReq

newtype AddFavoriteResponse = AddFavoriteResponse {addLineItemToFavoriteList :: FavoriteList}
  deriving stock (Generic, Show, Eq)
instance FromJSON AddFavoriteResponse

-- Basket (POST /baskets, POST /baskets/{id}/listings/{listingId})

newtype BasketId = BasketId Text deriving stock (Generic, Show, Eq)
instance FromJSON BasketId
instance ToJSON BasketId

newtype Qty = Qty Int deriving stock (Eq, Show, Generic)
instance FromJSON Qty
instance ToJSON Qty

newtype BasketVersion = BasketVersion Int deriving stock (Generic, Show, Eq)
instance FromJSON BasketVersion
instance ToJSON BasketVersion

data Item = Item {listingId :: ListingId, quantity :: Maybe Qty}

data Change = Change
  { id :: Text
  , message :: Text
  }
  deriving stock (Generic, Show, Eq)
instance FromJSON Change
instance ToJSON Change

data LineItem = LineItem
  { quantity :: Int
  , price :: CentPrice
  , totalPrice :: CentPrice
  , grammage :: Text
  , product :: Product
  , changes :: Maybe [Change]
  }
  deriving stock (Generic, Show, Eq)
instance FromJSON LineItem
instance ToJSON LineItem

data BasketSummary = BasketSummary
  { articleCount :: Int
  , articlePrice :: CentPrice
  , totalPrice :: CentPrice
  }
  deriving stock (Generic, Show, Eq)
instance FromJSON BasketSummary
instance ToJSON BasketSummary

data Staggering = Staggering
  { articlePriceThreshold :: CentPrice
  , displayText :: Text
  }
  deriving stock (Generic, Show, Eq)
instance FromJSON Staggering
instance ToJSON Staggering

data NextStaggering = NextStaggering
  { remainingArticlePrice :: CentPrice
  , articlePriceThreshold :: CentPrice
  , displayText :: Text
  }
  deriving stock (Generic, Show, Eq)
instance FromJSON NextStaggering
instance ToJSON NextStaggering

data Staggerings = Staggerings
  { reachedStaggering :: Staggering
  , nextStaggering :: Maybe NextStaggering
  }
  deriving stock (Generic, Show, Eq)
instance FromJSON Staggerings
instance ToJSON Staggerings

data TimeSlotInformation = TimeSlotInformation
  { startTime :: Maybe Text
  , endTime :: Maybe Text
  , timeSlotText :: Text
  }
  deriving stock (Generic, Show, Eq)
instance FromJSON TimeSlotInformation
instance ToJSON TimeSlotInformation

data ServiceSelection = ServiceSelection
  { wwIdent :: Text
  , serviceType :: Text
  , zipCode :: Text
  }
  deriving stock (Generic, Show, Eq)
instance FromJSON ServiceSelection
instance ToJSON ServiceSelection

data Basket = Basket
  { id :: BasketId
  , version :: BasketVersion
  , serviceSelection :: ServiceSelection
  , lineItems :: [LineItem]
  , summary :: BasketSummary
  , staggerings :: Staggerings
  , timeSlotInformation :: TimeSlotInformation
  , changes :: Maybe [Change]
  }
  deriving stock (Generic, Show, Eq)
instance FromJSON Basket
instance ToJSON Basket

newtype BasketResponse = BasketResponse {basket :: Basket}
  deriving stock (Generic, Show, Eq)
instance FromJSON BasketResponse
instance ToJSON BasketResponse

newtype BasketReq = BasketReq {includeTimeslot :: Bool} deriving stock (Show, Generic)
instance ToJSON BasketReq

data AddToBasketReq = AddToBasketReq
  { quantity :: Qty
  , basketVersion :: BasketVersion
  , includeTimeslot :: Bool
  }
  deriving stock (Show, Generic)
instance ToJSON AddToBasketReq

-- Checkout (POST /checkouts)

data CheckoutReq = CheckoutReq
  { basketId :: BasketId
  , loadBonusCredit :: Bool
  }
  deriving stock (Show, Generic)
instance ToJSON CheckoutReq

newtype CheckoutId = CheckoutId Text deriving stock (Generic, Show, Eq)
instance FromJSON CheckoutId
instance ToJSON CheckoutId

newtype CheckoutPayment = CheckoutPayment
  { paymentMethod :: Text
  }
  deriving stock (Generic, Show, Eq)
instance FromJSON CheckoutPayment
instance ToJSON CheckoutPayment

data CheckoutInfo = CheckoutInfo
  { id :: CheckoutId
  , basketId :: BasketId
  , marketId :: Text
  , zipCode :: Text
  , serviceType :: Text
  , isFreeOrder :: Bool
  , paymentType :: Text
  , timeslot :: Maybe Timeslot
  , payment :: Maybe CheckoutPayment
  }
  deriving stock (Generic, Show)
instance FromJSON CheckoutInfo
instance ToJSON CheckoutInfo

data CheckoutBasketSummary = CheckoutBasketSummary
  { id :: BasketId
  , version :: BasketVersion
  , summary :: BasketSummary
  }
  deriving stock (Generic, Show, Eq)
instance FromJSON CheckoutBasketSummary
instance ToJSON CheckoutBasketSummary

data CheckoutResponse = CheckoutResponse
  { checkout :: CheckoutInfo
  , basket :: CheckoutBasketSummary
  }
  deriving stock (Generic, Show)
instance FromJSON CheckoutResponse
instance ToJSON CheckoutResponse

newtype OrderId = OrderId Text deriving stock (Generic, Show, Eq)
instance FromJSON OrderId
instance ToJSON OrderId

newtype Order = Order {orderId :: OrderId}
  deriving stock (Generic, Show, Eq)
instance FromJSON Order
instance ToJSON Order

newtype OrderResponse = OrderResponse {order :: Order}
  deriving stock (Generic, Show, Eq)
instance FromJSON OrderResponse
instance ToJSON OrderResponse

newtype OrderCancelResponse = OrderCancelResponse {orderCancel :: Text}
  deriving stock (Generic, Show, Eq)
instance FromJSON OrderCancelResponse
instance ToJSON OrderCancelResponse

-- Order history (GET /orders/history)

data OrderTimeSlot = OrderTimeSlot
  { firstSlotDate :: Text
  , lastSlotDate :: Text
  }
  deriving stock (Generic, Show, Eq)
instance FromJSON OrderTimeSlot
instance ToJSON OrderTimeSlot

data SubOrder = SubOrder
  { isOpen :: Bool
  , status :: Text
  , timeSlot :: OrderTimeSlot
  , orderActions :: [Text]
  }
  deriving stock (Generic, Show, Eq)
instance FromJSON SubOrder
instance ToJSON SubOrder

data OrderHistoryEntry = OrderHistoryEntry
  { orderId :: OrderId
  , orderValue :: CentPrice
  , orderDate :: Text
  , subOrders :: [SubOrder]
  }
  deriving stock (Generic, Show, Eq)
instance FromJSON OrderHistoryEntry
instance ToJSON OrderHistoryEntry

newtype OrderHistory = OrderHistory
  { orders :: [OrderHistoryEntry]
  }
  deriving stock (Generic, Show, Eq)
instance FromJSON OrderHistory
instance ToJSON OrderHistory

newtype OrderHistoryResponse = OrderHistoryResponse {orderHistory :: OrderHistory}
  deriving stock (Generic, Show, Eq)
instance FromJSON OrderHistoryResponse
instance ToJSON OrderHistoryResponse

-- Order detail (GET /orders/{orderId})

data OrderDetailLineItem = OrderDetailLineItem
  { lineItemType :: Text
  , totalPrice :: CentPrice
  , productId :: Maybe ProductId
  , title :: Maybe Text
  , quantity :: Maybe Int
  , price :: Maybe CentPrice
  }
  deriving stock (Generic, Show, Eq)
instance FromJSON OrderDetailLineItem
instance ToJSON OrderDetailLineItem

data OrderDetailSubOrder = OrderDetailSubOrder
  { timeSlot :: OrderTimeSlot
  , status :: Text
  , lineItems :: [OrderDetailLineItem]
  }
  deriving stock (Generic, Show, Eq)
instance FromJSON OrderDetailSubOrder
instance ToJSON OrderDetailSubOrder

data OrderDetail = OrderDetail
  { orderId :: OrderId
  , orderDate :: Text
  , orderValue :: CentPrice
  , status :: Text
  , articlesPrice :: CentPrice
  , subOrders :: [OrderDetailSubOrder]
  }
  deriving stock (Generic, Show, Eq)
instance FromJSON OrderDetail
instance ToJSON OrderDetail

newtype OrderDetailResponse = OrderDetailResponse {orderDetails :: OrderDetail}
  deriving stock (Generic, Show, Eq)
instance FromJSON OrderDetailResponse
instance ToJSON OrderDetailResponse

data PatchTimeslotReq = PatchTimeslotReq
  { basketId :: BasketId
  , timeslotId :: TimeslotId
  }
  deriving stock (Show, Generic)
instance ToJSON PatchTimeslotReq

data PaymentOption = MarketPayment deriving stock (Show, Eq)
instance ToJSON PaymentOption where
  toJSON MarketPayment = String "MARKET_PAYMENT"

data PatchPaymentReq = PatchPaymentReq
  { basketId :: BasketId
  , paymentOption :: PaymentOption
  }
  deriving stock (Show, Generic)
instance ToJSON PatchPaymentReq

newtype ConfirmCheckoutReq = ConfirmCheckoutReq {basketId :: BasketId}
  deriving stock (Show, Generic)
instance ToJSON ConfirmCheckoutReq

-- Timeslots (GET /timeslots/checkout)

newtype TimeslotId = TimeslotId Text deriving stock (Generic, Show, Eq)
instance FromJSON TimeslotId
instance ToJSON TimeslotId

data Timeslot = Timeslot
  { id :: TimeslotId
  , startTime :: ZonedTime
  , endTime :: ZonedTime
  , serviceFee :: CentPrice
  }
  deriving stock (Generic, Show)
instance FromJSON Timeslot
instance ToJSON Timeslot

newtype FreeDeliveryInfo = FreeDeliveryInfo
  { freeDeliveryDays :: [Text]
  }
  deriving stock (Generic, Show, Eq)
instance FromJSON FreeDeliveryInfo
instance ToJSON FreeDeliveryInfo

data TimeslotsCheckoutResponse = TimeslotsCheckoutResponse
  { getTimeslotsCheckout :: [Timeslot]
  , freeDeliveryInfo :: Maybe FreeDeliveryInfo
  }
  deriving stock (Generic, Show)
instance FromJSON TimeslotsCheckoutResponse
instance ToJSON TimeslotsCheckoutResponse

-- Timeslot reservation (POST /timeslots/reservations)

newtype ReserveTimeslotReq = ReserveTimeslotReq {slotId :: TimeslotId}
  deriving stock (Show, Generic)
instance ToJSON ReserveTimeslotReq

data TimeslotReservation = TimeslotReservation
  { slotId :: TimeslotId
  , expireTime :: Text
  , slotStartTime :: Text
  , slotEndTime :: Text
  , fee :: Maybe CentPrice
  }
  deriving stock (Generic, Show, Eq)
instance FromJSON TimeslotReservation
instance ToJSON TimeslotReservation

newtype ReserveTimeslotResponse = ReserveTimeslotResponse {createTimeslotReservation :: TimeslotReservation}
  deriving stock (Generic, Show, Eq)
instance FromJSON ReserveTimeslotResponse
instance ToJSON ReserveTimeslotResponse

-- Generic response wrapper (all REWE responses wrap data in {"data": ...})

newtype ReweResponse a = ReweResponse {data_ :: a} deriving stock (Show, Generic)
instance (FromJSON a) => FromJSON (ReweResponse a) where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = \f -> if f == "data_" then "data" else f}
