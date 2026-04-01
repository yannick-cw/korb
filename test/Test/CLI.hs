module Test.CLI (spec) where

import Cli
import Data.Text (Text, unpack)
import Data.Text qualified as TIO
import Hedgehog (Gen, annotate, diff, forAll)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Options.Applicative (
  ParserResult (Success),
  defaultPrefs,
  execParserPure,
  info,
 )
import ReweApi.Types (
  EbonId (..),
  Item (..),
  ItemId (..),
  ListingId (..),
  OrderId (..),
  ProductId (..),
  Qty (..),
  SearchAttribute (..),
  TimeslotId (..),
 )
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec = describe "CLI" $ do
  it "roundtrips any command" $ hedgehog $ do
    command <- forAll genCommand
    let testParser = info commandParser mempty
    let parse = execParserPure defaultPrefs testParser
    let roundTripRes = parse (renderCommand command)
    case roundTripRes of
      Success cmd -> do
        annotate "the command == parse(render(command))"
        diff cmd (==) command
      _ -> fail "ouch - you better investigate"

renderCommand :: Command -> [String]
renderCommand cmd =
  unpack <$> case cmd of
    Store StoreShow -> ["store"]
    Store (StoreSearch (ZipCode z)) -> ["store", "search", z]
    Store (StoreSet (WwIdent w) (ZipCode z)) -> ["store", "set", w, z]
    Search q attrs -> ["search", q] ++ (attrs >>= renderAttr)
    Login -> ["login"]
    Slots -> ["timeslots"]
    Favorites FavoritesShow -> ["favorites"]
    Favorites (FavoritesFilter q) -> ["favorites", "search", q]
    Favorites (FavoritesAdd (ListingId l) (ProductId p)) -> ["favorites", "add", l, p]
    Favorites (FavoritesRemove (ItemId i)) -> ["favorites", "delete", i]
    Basket BasketShow -> ["basket"]
    Basket (BasketAdd (Item (ListingId l) Nothing)) -> ["basket", "add", l]
    Basket (BasketAdd (Item (ListingId l) (Just (Qty q)))) -> ["basket", "add", l, "--qty", TIO.show q]
    Ebon EbonShow -> ["ebons"]
    Ebon (EbonDownload (EbonId e) f) -> ["ebons", "download", e, "--output", TIO.pack f]
    Checkout GetCheckout -> ["checkout"]
    Checkout (StartCheckout (TimeslotId t)) -> ["checkout", "create", t]
    Checkout PlaceOrder -> ["checkout", "order"]
    Order (DeleteOrder (OrderId o)) -> ["orders", "delete", o]
    Order GetOrders -> ["orders"]
    Order OrdersHistory -> ["orders", "history"]
    Order (GetOrder (OrderId o)) -> ["orders", "get", o]
    Suggestion (ThresholdSuggestion (NumberOfSuggestions n)) -> ["suggestion", "threshold", TIO.show n]
  where
    renderAttr :: SearchAttribute -> [Text]
    renderAttr Organic = ["--organic"]
    renderAttr Regional = ["--regional"]
    renderAttr Vegan = ["--vegan"]
    renderAttr Vegetarian = ["--vegetarian"]

genCommand :: Gen Command
genCommand =
  Gen.choice
    [ Store <$> genStoreCommand
    , Search <$> genText <*> genSearchAttributes
    , pure Login
    , pure Slots
    , Favorites <$> genFavoritesCommand
    , Basket <$> genBasketCommand
    , Ebon <$> genEbonCommand
    , Checkout <$> genCheckoutCommand
    , Order <$> genOrderCommand
    , Suggestion <$> genSuggestionCommand
    ]

genStoreCommand :: Gen StoreCommand
genStoreCommand =
  Gen.choice
    [ pure StoreShow
    , StoreSearch <$> genZipCode
    , (StoreSet <$> genWwident) <*> genZipCode
    ]

genFavoritesCommand :: Gen FavoritesCommand
genFavoritesCommand =
  Gen.choice
    [ pure FavoritesShow
    , FavoritesFilter <$> genText
    , (FavoritesAdd <$> genListingId) <*> genProductId
    , FavoritesRemove <$> genItemId
    ]

genBasketCommand :: Gen BasketCommand
genBasketCommand =
  Gen.choice
    [ pure BasketShow
    , BasketAdd <$> genItem
    ]

genEbonCommand :: Gen EbonCommand
genEbonCommand =
  Gen.choice
    [ pure EbonShow
    , (EbonDownload <$> genEbonId) <*> (unpack <$> genText)
    ]

genCheckoutCommand :: Gen CheckoutCommand
genCheckoutCommand =
  Gen.choice
    [ pure GetCheckout
    , StartCheckout <$> genTimeslotId
    , pure PlaceOrder
    ]

genOrderCommand :: Gen OrderCommand
genOrderCommand =
  Gen.choice
    [ DeleteOrder <$> genOrderId
    , pure GetOrders
    , pure OrdersHistory
    , GetOrder <$> genOrderId
    ]

genSuggestionCommand :: Gen SuggestionCommand
genSuggestionCommand =
  ThresholdSuggestion . NumberOfSuggestions <$> Gen.int (Range.linear 0 100)

genReweId :: Gen Text
genReweId =
  Gen.text
    (Range.linear 5 50)
    (Gen.element ("abcdefghijklmnopqrstuvwxyz0123456789-" :: String))

genUUID :: Gen Text
genUUID = do
  a <- genUuidPart 8
  b <- genUuidPart 4
  c <- genUuidPart 4
  d <- genUuidPart 4
  e <- genUuidPart 12
  pure $ a <> "-" <> b <> "-" <> c <> "-" <> d <> "-" <> e
  where
    genUuidPart :: Int -> Gen Text
    genUuidPart numChars = Gen.text (Range.singleton numChars) (Gen.element ("0123456789abcdef" :: String))

genText :: Gen Text
genText = Gen.text (Range.linear 1 30) Gen.alphaNum

genZipCode :: Gen ZipCode
genZipCode = ZipCode <$> Gen.text (Range.singleton 5) Gen.digit

genWwident :: Gen WwIdent
genWwident = WwIdent <$> Gen.text (Range.linear 3 10) Gen.digit

genSearchAttributes :: Gen [SearchAttribute]
genSearchAttributes = Gen.subsequence [Organic, Regional, Vegan, Vegetarian]

genItemId :: Gen ItemId
genItemId = ItemId <$> genUUID

genListingId :: Gen ListingId
genListingId = ListingId <$> genReweId

genProductId :: Gen ProductId
genProductId = ProductId <$> genReweId

genEbonId :: Gen EbonId
genEbonId = EbonId <$> genUUID

genTimeslotId :: Gen TimeslotId
genTimeslotId = TimeslotId <$> genUUID

genOrderId :: Gen OrderId
genOrderId = OrderId <$> genReweId

genItem :: Gen Item
genItem = (Item <$> genListingId) <*> Gen.maybe (Qty <$> Gen.int (Range.linear 0 99))
