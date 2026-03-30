module ReweApi where

import Auth.Types (AccessToken (..), Auth (..))
import Cli (WwIdent (..), ZipCode (..))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (throwE)
import Data.Aeson (object)
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text, intercalate, isInfixOf, toLower)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (getCurrentTimeZone, utcToZonedTime, zonedTimeToUTC)
import Errors (ApiError (ApiError), AppError, IOE)
import HttpClient
import Network.HTTP.Req
import ReweApi.Types
import Storage

data ReweAuthedApi = ReweAuthedApi
  { getFavourites :: IOE ApiError (ReweResponse FavoritesResponse)
  , addFavourite ::
      FavoriteListId -> ListingId -> ProductId -> IOE ApiError (ReweResponse AddFavoriteResponse)
  , deleteFavourite :: FavoriteListId -> ItemId -> IOE ApiError ()
  , getBaseket :: IOE ApiError (ReweResponse BasketResponse)
  , getSlots :: IOE ApiError (ReweResponse TimeslotsCheckoutResponse)
  , addItemToBasket ::
      BasketId -> ListingId -> Qty -> BasketVersion -> IOE ApiError (ReweResponse BasketResponse)
  , postCheckout :: BasketId -> IOE ApiError (ReweResponse CheckoutResponse)
  , patchCheckoutTimeslot ::
      BasketId -> CheckoutId -> TimeslotId -> IOE ApiError (ReweResponse CheckoutResponse)
  , reserveTimeslot :: TimeslotId -> IOE ApiError (ReweResponse ReserveTimeslotResponse)
  , addPayment :: BasketId -> CheckoutId -> IOE ApiError (ReweResponse CheckoutResponse)
  , confirmOrder :: CheckoutId -> BasketId -> IOE ApiError (ReweResponse CheckoutResponse)
  , postOrder :: CheckoutId -> IOE ApiError (ReweResponse OrderResponse)
  , getOrders :: IOE ApiError (ReweResponse OrderHistoryResponse)
  , getOrder :: OrderId -> IOE ApiError (ReweResponse OrderDetailResponse)
  , deleteOrder :: OrderId -> IOE ApiError (ReweResponse OrderCancelResponse)
  }

newtype RewePublicApi = RewePublicApi
  { search :: Option 'Https -> IOE ApiError (ReweResponse SearchResponse)
  }

apiBase :: Url Https
apiBase = https "mobile-clients-api.rewe.de" /: "api"

mkRewePublicClient :: HttpClient -> CurrentStore -> RewePublicApi
mkRewePublicClient (HttpClient{get}) (CurrentStore (WwIdent wwIdent) (ZipCode zipCode)) =
  let mandatoryHeaders =
        header "rd-market-id" (encodeUtf8 wwIdent)
          <> header "rd-postcode" (encodeUtf8 zipCode)
          <> header "rd-service-types" "PICKUP"
   in RewePublicApi
        { search = \options -> get (apiBase /: "products") (options <> mandatoryHeaders)
        }

mkReweAuthedClient :: HttpClient -> Auth -> CurrentStore -> IOE AppError ReweAuthedApi
mkReweAuthedClient (HttpClient{get, post, delete, patch}) auth (CurrentStore (WwIdent wwIdent) (ZipCode zipCode)) = do
  (AccessToken tkn) <- auth.getValidToken
  let authHeader = oAuth2Bearer (encodeUtf8 tkn)
  let mandatoryHeaders =
        header "rd-market-id" (encodeUtf8 wwIdent)
          <> header "rd-postcode" (encodeUtf8 zipCode)
          <> header "rd-service-types" "PICKUP"
          <> authHeader
  pure
    ReweAuthedApi
      { getFavourites = get (apiBase /: "favorites") mandatoryHeaders
      , addFavourite = \(FavoriteListId favListId) listing productId ->
          post
            (AddFavoriteReq listing Nothing productId)
            (apiBase /: "favorites" /: favListId /: "lineitems")
            mandatoryHeaders
      , deleteFavourite = \(FavoriteListId favListId) (ItemId itemId) ->
          delete
            (apiBase /: "favorites" /: favListId /: "lineitems" /: itemId)
            mandatoryHeaders
      , getBaseket =
          post (BasketReq True) (apiBase /: "baskets") mandatoryHeaders
      , getSlots =
          get (apiBase /: "timeslots" /: "checkout") mandatoryHeaders
      , addItemToBasket = \(BasketId basketId) (ListingId listingId) qty version ->
          post
            (AddToBasketReq qty version True)
            (apiBase /: "baskets" /: basketId /: "listings" /: listingId)
            mandatoryHeaders
      , postCheckout = \basketId ->
          post (CheckoutReq basketId False) (apiBase /: "checkouts") mandatoryHeaders
      , patchCheckoutTimeslot = \basketId (CheckoutId checkoutId) timeslotId ->
          patch
            (PatchTimeslotReq basketId timeslotId)
            (apiBase /: "checkouts" /: checkoutId /: "timeslots")
            mandatoryHeaders
      , reserveTimeslot = \timeslotId -> post (ReserveTimeslotReq timeslotId) (apiBase /: "timeslots" /: "reservations") mandatoryHeaders
      , addPayment = \basketId (CheckoutId checkoutId) ->
          patch
            (PatchPaymentReq basketId MarketPayment)
            (apiBase /: "checkouts" /: checkoutId /: "payments")
            mandatoryHeaders
      , confirmOrder = \(CheckoutId checkoutId) basketId ->
          post
            (ConfirmCheckoutReq basketId)
            (apiBase /: "checkouts" /: checkoutId /: "confirmations")
            mandatoryHeaders
      , postOrder = \(CheckoutId checkoutId) ->
          post (object []) (apiBase /: "checkouts" /: checkoutId /: "orders") mandatoryHeaders
      , getOrders = get (apiBase /: "orders" /: "history") mandatoryHeaders
      , getOrder = \(OrderId orderId) -> get (apiBase /: "orders" /: orderId) mandatoryHeaders
      , deleteOrder = \(OrderId orderId) ->
          delete (apiBase /: "orders" /: orderId) mandatoryHeaders
      }

searchRewe :: RewePublicApi -> Text -> [SearchAttribute] -> IOE ApiError [Product]
searchRewe RewePublicApi{search} query attributes = do
  let filters = intercalate "&" ((\a -> "attribute=" <> attributeToText a) <$> attributes)
  searchRes <- search ("query" =: query <> "filters" =: filters)
  pure searchRes.data_.products.products

favorites :: ReweAuthedApi -> Maybe Text -> IOE ApiError [Product]
favorites ReweAuthedApi{getFavourites} query = do
  reweRes <- getFavourites
  let favs = reweRes.data_.favoriteLists.favorites >>= \fav -> fav.items
  pure $ maybe favs (\q -> filter (\p -> toLower q `isInfixOf` toLower p.title) favs) query

defaultFavoriteListId :: ReweAuthedApi -> IOE ApiError FavoriteListId
defaultFavoriteListId ReweAuthedApi{getFavourites} = do
  reweRes <- getFavourites
  case reweRes.data_.favoriteLists.favorites of
    (fav : _) -> pure fav.id
    [] -> throwE $ ApiError "No favorite list found"

favoritesAdd :: ReweAuthedApi -> ListingId -> ProductId -> IOE ApiError Product
favoritesAdd api@ReweAuthedApi{addFavourite} listing productId = do
  listId <- defaultFavoriteListId api
  reweRes <- addFavourite listId listing productId
  let products = reweRes.data_.addLineItemToFavoriteList.items
  let addedProduct = find (\p -> p.productId == productId) products
  maybe (throwE $ ApiError "Item was not added to favourites") pure addedProduct

favoritesDelete :: ReweAuthedApi -> ItemId -> IOE ApiError ()
favoritesDelete api@ReweAuthedApi{deleteFavourite} itemId = do
  listId <- defaultFavoriteListId api
  deleteFavourite listId itemId

baskets :: ReweAuthedApi -> IOE ApiError Basket
baskets ReweAuthedApi{getBaseket} = do
  reweRes <- getBaseket
  pure reweRes.data_.basket

basketsAdd :: ReweAuthedApi -> Item -> IOE ApiError (Maybe LineItem)
basketsAdd ReweAuthedApi{getBaseket, addItemToBasket} Item{listingId, quantity} = do
  currentBasket <- getBaseket
  let basket = currentBasket.data_.basket
  reweRes <- addItemToBasket basket.id listingId (fromMaybe (Qty 1) quantity) basket.version
  let wasDelete = any (\(Qty q) -> q <= 0) quantity
  let addedLineItem =
        find (\li -> li.product.listing.listingId == listingId) reweRes.data_.basket.lineItems
  maybe
    (if wasDelete then pure Nothing else throwE $ ApiError "Item was not added to basket")
    pure
    $ Just addedLineItem

slots :: ReweAuthedApi -> IOE ApiError TimeslotsCheckoutResponse
slots ReweAuthedApi{getSlots} = do
  timeslots <- (.data_) <$> getSlots
  localTZ <- liftIO getCurrentTimeZone
  let rezoneSlots ts =
        ts{startTime = updateZone localTZ ts.startTime, endTime = updateZone localTZ ts.endTime, id = ts.id}
  pure $ timeslots{getTimeslotsCheckout = rezoneSlots <$> timeslots.getTimeslotsCheckout}
 where
  updateZone localTz = utcToZonedTime localTz . zonedTimeToUTC

checkout :: ReweAuthedApi -> IOE ApiError CheckoutResponse
checkout api@ReweAuthedApi{postCheckout} = do
  basket <- baskets api
  when (null basket.lineItems) $ throwE (ApiError "Basket is empty - can't checkout an empty basket.")
  (.data_) <$> postCheckout basket.id

checkoutTimeslot :: ReweAuthedApi -> TimeslotId -> IOE ApiError CheckoutResponse
checkoutTimeslot api@ReweAuthedApi{patchCheckoutTimeslot, reserveTimeslot = reserve, addPayment} timeslot = do
  co <- checkout api
  reservedTimeslot <- (.data_.createTimeslotReservation) <$> reserve timeslot
  _ <- (.data_) <$> patchCheckoutTimeslot co.basket.id co.checkout.id reservedTimeslot.slotId
  (.data_) <$> addPayment co.basket.id co.checkout.id

orderCheckout :: ReweAuthedApi -> IOE ApiError OrderResponse
orderCheckout api@ReweAuthedApi{postOrder, confirmOrder} = do
  co <- checkout api
  when (null co.checkout.timeslot) $
    throwE (ApiError "Can't order a checkout that has no timeslot for pickup set.")
  _ <- confirmOrder co.checkout.id co.basket.id
  (.data_) <$> postOrder co.checkout.id

getOpenOrders :: ReweAuthedApi -> IOE ApiError [OrderHistoryEntry]
getOpenOrders api = do
  orders <- getOrderHistory api
  pure $ filter isActionable orders
 where
  isActionable order = any (\sub -> sub.isOpen && any (`elem` ["modify", "cancel"]) sub.orderActions) order.subOrders

getOrderHistory :: ReweAuthedApi -> IOE ApiError [OrderHistoryEntry]
getOrderHistory ReweAuthedApi{getOrders} = (.data_.orderHistory.orders) <$> getOrders

deleteOpenOrder :: ReweAuthedApi -> OrderId -> IOE ApiError OrderCancelResponse
deleteOpenOrder ReweAuthedApi{deleteOrder} orderId = (.data_) <$> deleteOrder orderId

getOneOrder :: ReweAuthedApi -> OrderId -> IOE ApiError OrderDetail
getOneOrder ReweAuthedApi{getOrder} orderId = (.data_.orderDetails) <$> getOrder orderId