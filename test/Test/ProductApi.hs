{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Test.ProductApi (spec) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Aeson (decode)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.Text (isInfixOf)
import Errors (ApiError (..))
import Hedgehog (Gen, annotate, diff, forAll)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import ReweApi
import ReweApi.Types
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
          ReweAuthedApi
            { getFavourites =
                pure ReweResponse{data_ = FavoritesResponse{favoriteLists = FavoriteLists{favorites = listings}}}
            , addFavourite = \_ _ _ -> throwE (ApiError "not implemented")
            , deleteFavourite = \_ _ -> throwE (ApiError "not implemented")
            , getBaseket = throwE (ApiError "not implemented")
            , getSlots = throwE (ApiError "not implemented")
            , addItemToBasket = \_ _ _ _ -> throwE (ApiError "not implemented")
            , postCheckout = \_ -> throwE (ApiError "not implemented")
            , patchCheckoutTimeslot = \_ _ _ -> throwE (ApiError "not implemented")
            , reserveTimeslot = \_ -> throwE (ApiError "not implemented")
            , addPayment = \_ _ -> throwE (ApiError "not implemented")
            , confirmOrder = \_ _ -> throwE (ApiError "not implemented")
            , postOrder = \_ -> throwE (ApiError "not implemented")
            , getOrders = throwE (ApiError "not implemented")
            , deleteOrder = \_ -> throwE (ApiError "not implemented")
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
    listingId <- forAll $ (.listingId) <$> genListing
    qty <- forAll $ Gen.maybe (Gen.int (Range.linear 1 10))
    let item = Item listingId (Qty <$> qty)
        staticLineItem listingAddId (Qty lineItemQty) =
          LineItem
            { quantity = lineItemQty
            , price = CentPrice 1
            , totalPrice = CentPrice 10
            , grammage = "1 Stück"
            , product = testProd{listing = testProd.listing{listingId = listingAddId}}
            , changes = Nothing
            }
        emptyBasket = mkEmptyBasket []
        testClient =
          ReweAuthedApi
            { getFavourites = throwE (ApiError "not implemented")
            , addFavourite = \_ _ _ -> throwE (ApiError "not implemented")
            , deleteFavourite = \_ _ -> throwE (ApiError "not implemented")
            , getBaseket = pure (ReweResponse (BasketResponse emptyBasket))
            , addItemToBasket = \_ pId lqty _ -> pure (ReweResponse (BasketResponse (mkEmptyBasket [staticLineItem pId lqty])))
            }
    res <- liftIO $ runExceptT $ basketsAdd testClient item
    case res of
      Right (Just li) -> do
        annotate "returned lineitem has the product id of the added product"
        diff li.product.listing.listingId (==) listingId
        annotate "quantity is the passed in quantity or default 1"
        diff li.quantity (==) (fromMaybe 1 qty)
      Right Nothing -> fail "impossible"
      Left err -> fail (show err)

genFavList :: Gen FavoriteList
genFavList = do
  products <- Gen.list (Range.linear 1 10) genProduct
  name <- Gen.text (Range.linear 1 10) Gen.alphaNum
  fid <- FavoriteListId <$> Gen.text (Range.linear 1 10) Gen.alphaNum
  pure FavoriteList{id = fid, name, items = products}

genProduct :: Gen Product
genProduct = do
  articleId <- Gen.text (Range.linear 1 10) Gen.alphaNum
  productId <- ProductId <$> Gen.text (Range.linear 1 10) Gen.alphaNum
  title <- Gen.text (Range.linear 1 50) Gen.unicode
  imageURL <- Gen.text (Range.linear 1 100) Gen.alphaNum
  orderLimit <- Gen.maybe (Gen.int (Range.linear 1 99))
  listing <- genListing
  pure Product{articleId, productId, title, imageURL, orderLimit, listing, attributes = Nothing, itemId = Nothing}

genListing :: Gen Listing
genListing = do
  listingId <- ListingId <$> Gen.text (Range.linear 1 20) Gen.alphaNum
  currentRetailPrice <- CentPrice <$> Gen.int (Range.linear 1 10000)
  grammage <- Gen.maybe (Gen.text (Range.linear 1 30) Gen.unicode)
  pure Listing{listingId, currentRetailPrice, grammage}

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