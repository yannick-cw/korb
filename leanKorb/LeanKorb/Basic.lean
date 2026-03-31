structure Product where
  productId : String
  title : String
  price : Nat
deriving Repr

structure Suggestion where
  product : Product
  freq : Nat
deriving Repr

def countOccurrences (id : String) (ids : List String) : Nat :=
  ids.foldl (init := 0) fun acc x => if x == id then acc + 1 else acc

def suggestionEngine
    (orderedProductIds : List String)
    (purchasedProducts : List Product)
    (basketProductIds : List String)
    (numSuggest : Nat) : List Suggestion :=
  let withFreq := purchasedProducts.filterMap fun p =>
    let freq := countOccurrences p.productId orderedProductIds
    if freq > 0 then some { product := p, freq := freq } else none
  let notInBasket := withFreq.filter fun s =>
    !basketProductIds.contains s.product.productId
  let sorted := notInBasket.mergeSort fun a b => a.freq >= b.freq
  sorted.take numSuggest

theorem freq_positive
    : ∀ (orderedProductIds : List String)
        (purchasedProducts : List Product)
        (basketProductIds : List String)
        (numSuggest : Nat)
        (s : Suggestion),
      s ∈ suggestionEngine orderedProductIds purchasedProducts basketProductIds numSuggest →
      s.freq > 0 := by
  intro orderedProductIds purchasedProducts basketProductIds numSuggest s hmem
  unfold suggestionEngine at hmem
  simp at hmem
  have h1 := List.mem_of_mem_take hmem
  have h2 := (List.mergeSort_perm ..).mem_iff.mp h1
  simp [List.mem_filter] at h2
  have h3 := h2.1
  simp at h3
  obtain ⟨p, _, hfreq, heq⟩ := h3
  rw [← heq]
  exact hfreq

private theorem countOccurrences_foldl_pos (id : String) (ids : List String) (acc : Nat)
    (h : ids.foldl (fun a x => if x == id then a + 1 else a) acc > acc)
    : id ∈ ids := by
  induction ids generalizing acc with
  | nil => simp [List.foldl] at h
  | cons x xs ih =>
    simp only [List.mem_cons, List.foldl] at h ⊢
    by_cases heq : (x == id) = true
    · left; exact eq_of_beq heq |>.symm
    · right; simp only [heq] at h; exact ih acc h

private theorem countOccurrences_pos_mem (id : String) (ids : List String)
    (h : countOccurrences id ids > 0) : id ∈ ids := by
  unfold countOccurrences at h
  exact countOccurrences_foldl_pos id ids 0 h

theorem suggestion_in_ordered_prods_and_purchased_prods
    : ∀ (orderedProductIds : List String)
        (purchasedProducts : List Product)
        (basketProductIds : List String)
        (numSuggest : Nat)
        (s : Suggestion),
      s ∈ suggestionEngine orderedProductIds purchasedProducts basketProductIds numSuggest →
      s.product.productId ∈ orderedProductIds ∧ s.product ∈ purchasedProducts := by
  intro orderedProductIds purchasedProducts basketProductIds numSuggest s hmem
  unfold suggestionEngine at hmem
  simp at hmem
  have h1 := List.mem_of_mem_take hmem
  have h2 := (List.mergeSort_perm ..).mem_iff.mp h1
  simp [List.mem_filter] at h2
  have h3 := h2.1
  simp at h3
  obtain ⟨p, hp_mem, hfreq, heq⟩ := h3
  rw [← heq]
  exact ⟨countOccurrences_pos_mem p.productId orderedProductIds hfreq, hp_mem⟩

theorem suggestion_not_in_basket
    : ∀ (orderedProductIds : List String)
        (purchasedProducts : List Product)
        (basketProductIds : List String)
        (numSuggest : Nat)
        (s : Suggestion),
      s ∈ suggestionEngine orderedProductIds purchasedProducts basketProductIds numSuggest →
      s.product.productId ∉ basketProductIds := by
  intro orderedProductIds purchasedProducts basketProductIds numSuggest s hmem
  unfold suggestionEngine at hmem
  simp at hmem
  have h1 := List.mem_of_mem_take hmem
  have h2 := (List.mergeSort_perm ..).mem_iff.mp h1
  simp [List.mem_filter] at h2
  have h3 := h2.2
  simp at h3
  exact h3

theorem length_less_eq_num_suggest
    (orderedProductIds : List String)
    (purchasedProducts : List Product)
    (basketProductIds : List String)
    (numSuggest : Nat)
    : (suggestionEngine orderedProductIds purchasedProducts basketProductIds numSuggest).length <= numSuggest := by
  unfold suggestionEngine
  exact List.length_take_le numSuggest _

private theorem freq_ge_trans (a b c : Suggestion)
    (hab : decide (a.freq >= b.freq) = true) (hbc : decide (b.freq >= c.freq) = true)
    : decide (a.freq >= c.freq) = true := by
  simp [decide_eq_true_eq] at *
  omega

private theorem freq_ge_total (a b : Suggestion)
    : (decide (a.freq >= b.freq) || decide (b.freq >= a.freq)) = true := by
  simp [decide_eq_true_eq, Bool.or_eq_true]
  omega

theorem suggestions_sorted_desc
    (orderedProductIds : List String)
    (purchasedProducts : List Product)
    (basketProductIds : List String)
    (numSuggest : Nat)
    : List.Pairwise (fun a b => a.freq ≥ b.freq)
        (suggestionEngine orderedProductIds purchasedProducts basketProductIds numSuggest) := by
  unfold suggestionEngine
  simp
  apply List.Pairwise.take
  have h := List.pairwise_mergeSort freq_ge_trans freq_ge_total
    (List.filter (fun s => !basketProductIds.contains s.product.productId)
      (List.filterMap (fun p =>
        let freq := countOccurrences p.productId orderedProductIds
        if freq > 0 then some { product := p, freq := freq } else none)
        purchasedProducts))
  simp [decide_eq_true_eq] at h
  exact h
