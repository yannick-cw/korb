module Main (main) where

import Test.Hspec
import Test.ProductApi qualified
import Test.StoreApi qualified

main :: IO ()
main = hspec $ do
  Test.StoreApi.spec
  Test.ProductApi.spec
