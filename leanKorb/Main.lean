import LeanKorb
import Lean.Data.Json

open Lean (Json FromJson ToJson fromJson? toJson)

instance : FromJson Product where
  fromJson? j := do
    let productId ← j.getObjValAs? String "productId"
    let title ← j.getObjValAs? String "title"
    let price ← j.getObjValAs? Nat "price"
    pure { productId, title, price }

instance : ToJson Product where
  toJson p := Json.mkObj [
    ("productId", toJson p.productId),
    ("title", toJson p.title),
    ("price", toJson p.price)
  ]

instance : ToJson Suggestion where
  toJson s := Json.mkObj [
    ("product", toJson s.product),
    ("freq", toJson s.freq)
  ]

structure Input where
  orderedProductIds : List String
  purchasedProducts : List Product
  basketProductIds : List String
  numSuggest : Nat

instance : FromJson Input where
  fromJson? j := do
    let orderedProductIds ← j.getObjValAs? (List String) "orderedProductIds"
    let purchasedProducts ← j.getObjValAs? (List Product) "purchasedProducts"
    let basketProductIds ← j.getObjValAs? (List String) "basketProductIds"
    let numSuggest ← j.getObjValAs? Nat "numSuggest"
    pure { orderedProductIds, purchasedProducts, basketProductIds, numSuggest }

def main : IO Unit := do
  let stdin ← IO.getStdin
  let input ← stdin.readToEnd
  match Json.parse input >>= fromJson? (α := Input) with
  | .error err => IO.eprintln s!"JSON parse error: {err}"
  | .ok inp =>
    let result := suggestionEngine inp.orderedProductIds inp.purchasedProducts inp.basketProductIds inp.numSuggest
    IO.println (toString (toJson result))
