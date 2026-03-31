{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Test.ProductApi (spec) where

import Cli (NumberOfSuggestions (NumberOfSuggestions))
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Aeson (decode)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (Foldable (toList), for_)
import Data.List (find, sortOn)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Text (isInfixOf)
import Data.Time (ZonedTime (..), getCurrentTimeZone, utc, utcToZonedTime)
import Errors (ApiError (..))
import Hedgehog (annotate, diff, forAll)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import ReweApi
import ReweApi.Types
import Test.Helpers
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec = describe "ProductApi" $ do
  it "decodes search response" $ do
    jsonTestFile <- BL.readFile "./test/search_response.json"
    case decode jsonTestFile :: Maybe SearchProducts of
      Nothing -> expectationFailure "Failed to decode search response"
      Just (SearchProducts products) -> length products `shouldBe` 14

  it "filters by all queries" $ hedgehog $ do
    listings <- forAll $ Gen.list (Range.linear 1 10) genFavList
    toQueryTitle <- forAll $ Gen.element (listings >>= \l -> (.title) <$> l.items)
    let testClient =
          failingClient
            { getFavourites =
                pure
                  ReweResponse
                    { data_ = FavoritesResponse{favoriteLists = FavoriteLists{favorites = listings}}
                    }
            }
    res <- liftIO $ runExceptT $ favorites testClient (Just toQueryTitle)
    case res of
      Left err -> fail (show err)
      Right products -> do
        annotate "filtered products are always <= original products"
        diff (length products) (<=) (length (listings >>= \l -> l.items))

        annotate "every result contains the query"
        for_ products $ \p -> diff toQueryTitle isInfixOf p.title

  it "returns the lineitem with the added qty" $ hedgehog $ do
    testProd <- forAll genProduct
    lineItem <- forAll $ genLineItem testProd
    qty <- forAll $ Gen.maybe (Gen.int (Range.linear 1 10))
    let item = Item{listingId = testProd.listing.listingId, quantity = Qty <$> qty}
        emptyBasket = mkEmptyBasket []
        testClient =
          failingClient
            { getBaseket = pure (ReweResponse (BasketResponse emptyBasket))
            , addItemToBasket = \_ _ (Qty lqty) _ -> pure (ReweResponse (BasketResponse (mkEmptyBasket [lineItem{quantity = lqty}])))
            }
    res <- liftIO $ runExceptT $ basketsAdd testClient item
    case res of
      Right (Just li) -> do
        annotate "quantity is the passed in quantity or default 1"
        diff li.quantity (==) (fromMaybe 1 qty)
      Right Nothing -> fail "impossible"
      Left err -> fail (show err)

  it "always adds the local timezone to timeslots" $ hedgehog $ do
    testStartTime <- forAll $ utcToZonedTime utc <$> genZoneTime
    testEndTime <- forAll $ utcToZonedTime utc <$> genZoneTime
    localTimeZone <- liftIO getCurrentTimeZone
    let testClient =
          failingClient
            { getSlots =
                pure $
                  ReweResponse
                    ( TimeslotsCheckoutResponse
                        { getTimeslotsCheckout =
                            [Timeslot (TimeslotId "a") testStartTime testEndTime (CentPrice 22)]
                        , freeDeliveryInfo = Nothing
                        }
                    )
            }
    res <- liftIO $ runExceptT $ slots testClient
    case res of
      Right TimeslotsCheckoutResponse{getTimeslotsCheckout = [slot]} -> do
        annotate "slots times are in local timezone"
        diff slot.startTime.zonedTimeZone (==) localTimeZone
        diff slot.endTime.zonedTimeZone (==) localTimeZone
      Right _ -> fail "impossible"
      Left err -> fail (show err)

  it "suggestions filter to only existing products not in basket" $ hedgehog $ do
    products <- forAll $ Gen.nonEmpty (Range.linear 1 100) genProduct
    pruchableProducts <- forAll $ Gen.subsequence (toList products)
    lineItemInBasket <- forAll $ Gen.element products >>= genLineItem
    orderProductsSets <-
      forAll $ Gen.list (Range.linear 1 10) (Gen.subsequence $ toList products)
    (orderHistoryEntries, orderDetails) <-
      forAll $ unzip <$> forM orderProductsSets genOrderHistory
    numSuggestions <- forAll $ Gen.int (Range.linear 0 100)
    let testClient =
          mkSuggestionClient
            (mkEmptyBasket [lineItemInBasket])
            orderHistoryEntries
            orderDetails
            pruchableProducts
    res <-
      liftIO $ runExceptT $ thresholdSuggestion testClient (NumberOfSuggestions numSuggestions)
    case res of
      Left err -> fail (show err)
      Right (SuggestionResponse{suggestions}) -> do
        annotate "Never suggest never ordered prodct"
        for_ suggestions $ \s -> diff s.freq (>) 0
        annotate "Never suggest more than numSuggestion"
        diff (length suggestions) (<=) numSuggestions
        annotate "Never suggest items already in the basket"
        for_ suggestions $ \s -> diff s.product.productId (/=) lineItemInBasket.product.productId
        annotate "Always sort suggesions with most frequently bought first"
        let freqs = (.freq) <$> suggestions
        diff freqs (==) (sortOn Down freqs)
        annotate "All suggestions must be purchable"
        for_ suggestions $ \s -> diff s.product.productId elem ((.productId) <$> pruchableProducts)
  where
    mkSuggestionClient ::
      Basket -> [OrderHistoryEntry] -> [OrderDetail] -> [Product] -> ReweAuthedApi
    mkSuggestionClient currentBasket entries details purchased =
      failingClient
        { getBaseket = pure $ ReweResponse (BasketResponse currentBasket)
        , getOrders = \_ -> pure $ ReweResponse (OrderHistoryResponse (OrderHistory entries))
        , getOrder = \oid ->
            maybe
              (throwE $ ApiError "Order not found")
              (pure . ReweResponse . OrderDetailResponse)
              (find (\o -> o.orderId == oid) details)
        , getPurchasedProducts =
            pure $ ReweResponse (PurchasedProductsResponse (SearchProducts purchased))
        }

mkEmptyBasket :: [LineItem] -> Basket
mkEmptyBasket items =
  Basket
    { id = BasketId "test-basket"
    , version = BasketVersion 0
    , serviceSelection = ServiceSelection "440340" "PICKUP" "82418"
    , lineItems = items
    , summary = BasketSummary 0 (CentPrice 0) (CentPrice 0)
    , staggerings = Staggerings (Staggering (CentPrice 0) "") Nothing
    , timeSlotInformation = TimeSlotInformation Nothing Nothing ""
    , changes = Nothing
    }

failingClient :: ReweAuthedApi
failingClient =
  ReweAuthedApi
    { getFavourites = throwE (ApiError "not implemented")
    , addFavourite = \_ _ _ -> throwE (ApiError "not implemented")
    , deleteFavourite = \_ _ -> throwE (ApiError "not implemented")
    , getBaseket = throwE (ApiError "not implemented")
    , addItemToBasket = \_ _ _ _ -> throwE (ApiError "not implemented")
    , getSlots = throwE (ApiError "not implemented")
    , postCheckout = \_ -> throwE (ApiError "not implemented")
    , patchCheckoutTimeslot = \_ _ _ -> throwE (ApiError "not implemented")
    , reserveTimeslot = \_ -> throwE (ApiError "not implemented")
    , addPayment = \_ _ -> throwE (ApiError "not implemented")
    , confirmOrder = \_ _ -> throwE (ApiError "not implemented")
    , postOrder = \_ -> throwE (ApiError "not implemented")
    , getOrders = \_ -> throwE (ApiError "not implemented")
    , getOrder = \_ -> throwE (ApiError "not implemented")
    , deleteOrder = \_ -> throwE (ApiError "not implemented")
    , getEbons = throwE (ApiError "not implemented")
    , getReceipt = \_ -> throwE (ApiError "not implemented")
    , getPurchasedProducts = throwE (ApiError "not implemented")
    }
