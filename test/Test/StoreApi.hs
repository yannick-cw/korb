module Test.StoreApi (spec) where

import Data.Aeson (decode, encode)
import StoreApi (PickupMarket (..))
import Test.Hspec

spec :: Spec
spec = describe "StoreApi" $ do
  it "roundtrips PickupMarket json" $ do
    let store = PickupMarket "Test Store" "AlabamaStore" "Alabama" "3243" "PICKUP"
        decodedStore :: Maybe PickupMarket = decode (encode store)
    decodedStore `shouldBe` Just store
