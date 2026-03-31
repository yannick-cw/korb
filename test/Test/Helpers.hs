{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Helpers where

import Data.Time (UTCTime (..), fromGregorian)
import Data.Time.Clock (secondsToDiffTime)
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import ReweApi.Types

genZoneTime :: Gen UTCTime
genZoneTime = do
  y <- toInteger <$> Gen.int (Range.constant 2000 2019)
  m <- Gen.int (Range.constant 1 12)
  d <- Gen.int (Range.constant 1 28)
  let day = fromGregorian y m d
  secs <- toInteger <$> Gen.int (Range.constant 0 86401)
  let timeDiff = secondsToDiffTime secs
  pure $ UTCTime day timeDiff

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
  pure
    Product
      { articleId
      , productId
      , title
      , imageURL
      , orderLimit
      , listing
      , attributes = Nothing
      , itemId = Nothing
      }

genListing :: Gen Listing
genListing = do
  listingId <- ListingId <$> Gen.text (Range.linear 1 20) Gen.alphaNum
  currentRetailPrice <- CentPrice <$> Gen.int (Range.linear 1 10000)
  grammage <- Gen.maybe (Gen.text (Range.linear 1 30) Gen.unicode)
  pure Listing{listingId, currentRetailPrice, grammage}

genOrderHistory :: [Product] -> Gen (OrderHistoryEntry, OrderDetail)
genOrderHistory products = do
  orderId <- OrderId <$> Gen.text (Range.linear 5 15) Gen.alphaNum
  orderDate <- Gen.text (Range.linear 10 20) Gen.digit
  let orderValue = sum $ (.listing.currentRetailPrice) <$> products
  status <- Gen.element ["open", "closed", "cancelled"]
  (firstSlotDate, lastSlotDate) <- liftA2 (,) genZoneTime genZoneTime
  let detailLineItems =
        ( \p ->
            OrderDetailLineItem
              { lineItemType = "PRODUCT"
              , totalPrice = p.listing.currentRetailPrice
              , productId = Just p.productId
              , title = Just p.title
              , quantity = Just 1
              , price = Just p.listing.currentRetailPrice
              }
        )
          <$> products
  let historyEntry =
        OrderHistoryEntry
          { orderId
          , orderValue
          , orderDate
          , subOrders =
              [ SubOrder
                  { isOpen = status == "open"
                  , status
                  , timeSlot = OrderTimeSlot{firstSlotDate, lastSlotDate}
                  , orderActions = if status == "open" then ["modify", "cancel"] else []
                  }
              ]
          }
  let detail =
        OrderDetail
          { orderId
          , orderDate
          , orderValue
          , status
          , articlesPrice = orderValue
          , subOrders =
              [ OrderDetailSubOrder
                  { timeSlot = OrderTimeSlot{firstSlotDate, lastSlotDate}
                  , status
                  , lineItems = detailLineItems
                  }
              ]
          }
  pure (historyEntry, detail)

genLineItem :: Product -> Gen LineItem
genLineItem prod = do
  qty <- Gen.int (Range.constant 1 10)
  pure
    LineItem
      { quantity = qty
      , price = CentPrice 1
      , totalPrice = CentPrice 10
      , grammage = "1 Stück"
      , product = prod
      , changes = Nothing
      }
