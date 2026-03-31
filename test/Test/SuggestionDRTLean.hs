module Test.SuggestionDRTLean (spec) where

import Cli (NumberOfSuggestions (NumberOfSuggestions))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Foldable (Foldable (toList))
import Data.Text (Text)
import GHC.Generics (Generic)
import Hedgehog (annotate, diff, forAll)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import ReweApi
import ReweApi.Types
import System.Process (readProcess)
import Test.Helpers
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

data LeanProd = LeanProd
  { productId :: ProductId
  , title :: Text
  , price :: CentPrice
  }
  deriving stock (Generic, Show, Eq)
instance FromJSON LeanProd
instance ToJSON LeanProd
data LeanFormat = LeanFormat
  { orderedProductIds :: [ProductId]
  , purchasedProducts :: [LeanProd]
  , basketProductIds :: [ProductId]
  , numSuggest :: Int
  }
  deriving stock (Generic, Show)
instance ToJSON LeanFormat

data LeanRes = LeanRes {product :: LeanProd, freq :: Int} deriving stock (Generic, Show)
instance FromJSON LeanRes

readLeanBridge :: String -> IO String
readLeanBridge = readProcess "leanKorb/.lake/build/bin/leankorb" []

spec :: Spec
spec = describe "Suggestion Lean DRT" $ do
  it "lean matches haskell engine" $ hedgehog $ do
    products <- forAll $ Gen.nonEmpty (Range.linear 1 100) genProduct
    purchasedProducts <- forAll $ Gen.subsequence (toList products)
    basketProductIds <- forAll $ Gen.subsequence ((.productId) <$> toList products)
    orderedProductIds <- -- ensures duplicates are likely
      forAll $ Gen.list (Range.linear 1 500) (Gen.element ((.productId) <$> toList products))
    numSuggest <- forAll $ Gen.int (Range.linear 0 100)
    let num = NumberOfSuggestions numSuggest
    let leanInput =
          LeanFormat
            { orderedProductIds
            , purchasedProducts =
                (\p -> LeanProd p.productId p.title p.listing.currentRetailPrice) <$> purchasedProducts
            , basketProductIds = basketProductIds
            , numSuggest
            }
    let suggestions = suggestionEngine orderedProductIds purchasedProducts basketProductIds num
    leanSuggestions <- liftIO $ leanOutToRes <$> readLeanBridge (leanInToText leanInput)
    annotate "suggestions are the same in the same order"
    diff
      ((\s -> (s.product.productId, s.freq)) <$> suggestions)
      (==)
      ((\l -> (l.product.productId, l.freq)) <$> leanSuggestions)
  where
    leanInToText :: LeanFormat -> String
    leanInToText = BL.unpack . encode
    leanOutToRes :: String -> [LeanRes]
    leanOutToRes leanOut = case eitherDecode (BL.pack leanOut) of
      Left err -> error ("Lean output decode failed: " <> err)
      Right res -> res
