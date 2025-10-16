{-# OPTIONS --without-K --guardedness  --safe  #-}

module Derivation where

{-
  We formalize the following parts from derivation.ml
  1) The basic interface of stream operations "stream2".
  2) The type of the stateful stream "ststream".
  3) An implementation of the stateful stream "ststream" that follows the interface defined in "stream2".
  4) Types for Strong-Weak bisimulations and provide proofs that they form an equivalence relation.
  5) Proofs for all the specified equation laws among streams.
-}

open import Relation.Binary.PropositionalEquality
open import Relation.Binary.PropositionalEquality.Core
open import Function
open import Data.Product
open import Data.Sum
open import Data.Bool using (Bool; true; false; _∧_; _∨_; if_then_else_)
open import Data.Nat using (ℕ; zero; suc; _≤_; z≤n; s≤s; _+_; Ordering; less; equal; greater; compare)
open import Data.List using (List; _∷_; [])
open import Data.Vec using (Vec; _∷_; [])
open import Data.Fin using (Fin; zero; suc)
open import Data.Maybe using (Maybe; just; nothing)
open import Data.Empty using (⊥; ⊥-elim)
open import Data.Unit using (⊤; tt)


record Stream₂ : Set₁ where
  field
    Stream : Set → Set → Set
    unroll : ∀ {A Z : Set} → (Z → Maybe A × Z) → Z → Stream A Z
    pullArray : ∀ {A Z : Set} → Z → (n : ℕ) → (Z → Fin n → A × Z) → Stream A Z
    initSt : ∀ {A Z Z₁ : Set} → Z → Stream A Z₁ → Stream A (Z × Z₁)
    mapFilterSt : ∀ {A B Z : Set} → (Z → A → Maybe B × Z) → Stream A Z → Stream B Z 
    guard : ∀ {A Z : Set} → (Z → Bool) → Stream A Z → Stream A Z
    zipSt : ∀ {A B Z₁ Z₂ : Set} → Stream A Z₁ → Stream B Z₂ → Stream (A × B) (Z₁ × Z₂)
    flatmapSt : ∀ {A B Z : Set} → (Z → A → Stream B Z) → Stream A Z → Stream B Z
    adjust : ∀ {A Z₁ Z₂ : Set} → (Z₁ → Z₂) × (Z₂ → Z₁) → Stream A Z₁ → Stream A Z₂
    abstractSt : ∀ {A Zp Z : Set} → Stream A (Zp × Z) → Stream A Z
    observe : ∀ {A Z : Set} → ℕ → Stream A Z → List A

mutual 

  record StStream (A : Set) (Z : Set) : Set where
    constructor mkStream
    coinductive
    field
      stream : StStreamF A Z
      
  data StStreamF (A : Set) (Z : Set) : Set where
    nil : StStreamF A Z 
    cons : (Maybe A × Z) → (Z → StStream A Z) → StStreamF A Z 

      
module StStreamFunctions where

  unroll : ∀ {A Z : Set} → (Z → Maybe A × Z) → Z → StStream A Z
  StStream.stream (unroll ustep z) = cons (ustep z) (unroll ustep)

  pullArray : ∀ {A Z : Set} → Z → (n : ℕ) → (Z → Fin n → A × Z) → StStream A Z
  StStream.stream (pullArray z zero step) = nil 
  StStream.stream (pullArray z (suc n) step) with step z zero
  ... | x , z' = cons (just x , z') (λ z → pullArray z n (λ z n → step z (suc n)))

  initSt : ∀ {A Z Z₁ : Set} → Z → StStream A Z₁ → StStream A (Z × Z₁)
  StStream.stream (initSt {A} {Z} {Z₁} z st) with StStream.stream st
  ... | nil = nil
  ... | cons (a , z₁) t = cons (a , (z , z₁)) (λ { (z , z₁) → initSt z (t z₁) })

  mapFilterSt : ∀ {A B Z : Set} → (Z → A → Maybe B × Z) → StStream A Z → StStream B Z
  StStream.stream (mapFilterSt f st) with StStream.stream st
  ... | nil = nil
  ... | cons (just a , z) t = cons (f z a) (λ z → mapFilterSt f (t z))
  ... | cons (nothing , z) t = cons (nothing , z) (λ z → mapFilterSt f (t z))

  guard : ∀ {A Z : Set} → (Z → Bool) → StStream A Z → StStream A Z
  StStream.stream (guard p st) with StStream.stream st
  StStream.stream (guard p st) | nil = nil
  StStream.stream (guard p st) | cons (a , z) t with p z
  StStream.stream (guard p st) | cons (a , z) t | false = nil
  StStream.stream (guard p st) | cons (a , z) t | true = cons (a , z) (λ z → guard p (t z))

  mutual 
    flatmapSt : ∀ {A B Z : Set} → (Z → A → StStream B Z) → StStream A Z → StStream B Z
    StStream.stream (flatmapSt f st) with StStream.stream st
    ... | nil = nil
    ... | cons (just a , z) t = cons (nothing , z) (λ z → flatmapInnerSt f z t (f z a))
    ... | cons (nothing , z) t = cons (nothing , z) (λ z → flatmapSt f (t z))

    flatmapInnerSt : ∀ {A B Z : Set} → (Z → A → StStream B Z) → Z → (Z → StStream A Z) → StStream B Z → StStream B Z
    StStream.stream (flatmapInnerSt f z stouter st) with StStream.stream st
    ... | nil = cons (nothing , z) (λ z → flatmapSt f (stouter z))
    ... | cons bz t = cons bz (λ z → flatmapInnerSt f z stouter (t z))

  zipSt : ∀ {A B Z₁ Z₂ : Set} → StStream A Z₁ → StStream B Z₂ → StStream (A × B) (Z₁ × Z₂)
  StStream.stream (zipSt st₁ st₂) with StStream.stream st₁
  StStream.stream (zipSt st₁ st₂) | nil = nil
  StStream.stream (zipSt st₁ st₂) | cons (just a , z₁) t₁ with StStream.stream st₂
  StStream.stream (zipSt st₁ st₂) | cons (just a , z₁) t₁ | nil = nil
  StStream.stream (zipSt st₁ st₂) | cons (just a , z₁) t₁ | cons (just b , z₂) t₂ = cons (just (a , b) , (z₁ , z₂)) (λ { (z₁ , z₂) → zipSt (t₁ z₁) (t₂ z₂) })
  StStream.stream (zipSt st₁ st₂) | cons (just a , z₁) t₁ | cons (nothing , z₂) t₂ = cons (nothing , (z₁ , z₂)) (λ { (z₁ , z₂) → zipSt st₁ (t₂ z₂) })
  StStream.stream (zipSt st₁ st₂) | cons (nothing , z₁) t₁ with StStream.stream st₂
  StStream.stream (zipSt st₁ st₂) | cons (nothing , z₁) t₁ | nil = nil
  StStream.stream (zipSt st₁ st₂) | cons (nothing , z₁) t₁ | cons (just b , z₂) t₂ = cons (nothing , (z₁ , z₂)) (λ { (z₁ , z₂) → zipSt (t₁ z₁) st₂ })
  StStream.stream (zipSt st₁ st₂) | cons (nothing , z₁) t₁ | cons (nothing , z₂) t₂ = cons (nothing , (z₁ , z₂)) (λ { (z₁ , z₂) → zipSt (t₁ z₁) (t₂ z₂) })

  adjust : ∀ {A Z₁ Z₂ : Set} → ((Z₁ → Z₂) × (Z₂ → Z₁)) → StStream A Z₁ → StStream A Z₂
  StStream.stream (adjust (f , g) st) with StStream.stream st
  ... | nil = nil
  ... | cons (a , z₁) t = cons (a , f z₁) (λ z₂ → adjust (f , g) (t (g z₂)))

  abstractSt : ∀ {A Zp Z : Set} →  StStream A (Zp × Z) → StStream A Z
  StStream.stream (abstractSt st) with StStream.stream st
  ... | nil = nil
  ... | cons (a , (zp , z)) t = cons (a , z) (λ z → abstractSt (t (zp , z)))

  observe : ∀ {A Z : Set} → ℕ → StStream A Z → List A
  observe zero st = []
  observe (suc n) st with StStream.stream st
  ... | nil = []
  ... | cons (just a , z) t = a ∷ observe n (t z)
  ... | cons (nothing , z) t = observe n (t z)
  

stream2Denot : Stream₂
Stream₂.Stream stream2Denot = StStream
Stream₂.unroll stream2Denot = StStreamFunctions.unroll
Stream₂.pullArray stream2Denot = StStreamFunctions.pullArray
Stream₂.initSt stream2Denot = StStreamFunctions.initSt
Stream₂.mapFilterSt stream2Denot = StStreamFunctions.mapFilterSt
Stream₂.guard stream2Denot = StStreamFunctions.guard
Stream₂.zipSt stream2Denot = StStreamFunctions.zipSt
Stream₂.flatmapSt stream2Denot = StStreamFunctions.flatmapSt
Stream₂.adjust stream2Denot = StStreamFunctions.adjust
Stream₂.abstractSt stream2Denot = StStreamFunctions.abstractSt
Stream₂.observe stream2Denot = StStreamFunctions.observe


-- Strong Bisimulation
mutual
  record StStream≅ {A Z : Set} (st st' : StStream A Z) : Set where
    constructor mkStream≅
    coinductive
    field
      stream : StStreamEq A Z (StStream.stream st) (StStream.stream st')

  data StStreamEq (A Z : Set) : StStreamF A Z → StStreamF A Z → Set where
    eqnil : StStreamEq A Z nil nil
    eqskip : {z₁ z₂ : Z} → {t₁ t₂ : Z → StStream A Z} → StStream≅ (t₁ z₁) (t₂ z₂) → StStreamEq A Z (cons (nothing , z₁) t₁) (cons (nothing , z₂) t₂)
    eqcons : {a₁ a₂ : A} → (eq : a₁ ≡ a₂) → {z₁ z₂ : Z} → {t₁ t₂ : Z → StStream A Z} → StStream≅ (t₁ z₁) (t₂ z₂) → StStreamEq A Z (cons (just a₁ , z₁) t₁) (cons (just a₂ , z₂) t₂)

  refl≅ : ∀ {A Z : Set} {st : StStream A Z} → StStream≅ st st
  StStream≅.stream (refl≅ {st = st}) with StStream.stream st
  StStream≅.stream (refl≅ {st = st}) | nil = eqnil
  StStream≅.stream (refl≅ {st = st}) | cons (just x , z) tl = eqcons refl refl≅
  StStream≅.stream (refl≅ {st = st}) | cons (nothing , z) tl = eqskip refl≅
  
  sym≅ : ∀ {A Z : Set} {st st' : StStream A Z} → StStream≅ st st' → StStream≅ st' st
  StStream≅.stream (sym≅ {st = st} {st' = st'} eq) = sym≅' (StStream.stream st) (StStream.stream st') (StStream≅.stream eq)

  sym≅' : ∀ {A Z : Set} (st st' : StStreamF A Z) → StStreamEq A Z st st' → StStreamEq A Z st' st
  sym≅' nil nil eq = eqnil
  sym≅' (cons (just x₁ , z₁) tl₁) (cons (just .x₁ , z₂) tl₂) (eqcons refl eq) = eqcons refl (sym≅ eq)
  sym≅' (cons (nothing , z₁) tl₁) (cons (nothing , z₂) tl₂) (eqskip eq) = eqskip (sym≅ eq)

  trans≅ : ∀ {A Z : Set} {st₁ st₂ st₃ : StStream A Z} → StStream≅ st₁ st₂ → StStream≅ st₂ st₃ → StStream≅ st₁ st₃
  StStream≅.stream (trans≅ {st₁ = st₁} {st₂ = st₂} {st₃ = st₃} eq₁ eq₂) =  trans≅' (StStream.stream st₁) (StStream.stream st₂) (StStream.stream st₃) (StStream≅.stream eq₁) (StStream≅.stream eq₂)
  
  trans≅' : ∀ {A Z : Set} (st₁ st₂ st₃ : StStreamF A Z) → StStreamEq A Z st₁ st₂ → StStreamEq A Z st₂ st₃ → StStreamEq A Z st₁ st₃
  trans≅' nil nil nil eq₁ eq₂ = eqnil
  trans≅' (cons (just x₁ , z₁) tl₁) (cons (just .x₁ , z₂) tl₂) (cons (just .x₁ , z₃) tl₃) (eqcons refl eq₁) (eqcons refl eq₂) = eqcons refl (trans≅ eq₁ eq₂)
  trans≅' (cons (nothing , z₁) tl₁) (cons (nothing , z₂) tl₂) (cons (nothing , z₃) tl₃) (eqskip eq₁) (eqskip eq₂) = eqskip (trans≅ eq₁ eq₂)
    
module _ where 
  open import Relation.Binary
  setoidStStream≅ : ∀ (A Z : Set) → Setoid _ _
  Setoid.Carrier (setoidStStream≅ A Z) = StStream A Z
  Setoid._≈_ (setoidStStream≅ A Z) = StStream≅
  IsEquivalence.refl (Setoid.isEquivalence (setoidStStream≅ A Z)) = refl≅
  IsEquivalence.sym (Setoid.isEquivalence (setoidStStream≅ A Z)) = sym≅
  IsEquivalence.trans (Setoid.isEquivalence (setoidStStream≅ A Z)) = trans≅ 

-- Weak Bisimulation
mutual
  record StStream≈ {A Z : Set} (st st' : StStream A Z) : Set where
    constructor mkStream≈
    coinductive
    field
      stream : StStreamWEq A Z (StStream.stream st) (StStream.stream st')
      
  
  data StStreamWEq (A Z : Set) : StStreamF A Z → StStreamF A Z → Set where
    eqnil : StStreamWEq A Z nil nil
    eqskip : {z₁ z₂ : Z} → {t₁ t₂ : Z → StStream A Z} → StStream≈ (t₁ z₁) (t₂ z₂) → StStreamWEq A Z (cons (nothing , z₁) t₁) (cons (nothing , z₂) t₂)
    eqcons : {a₁ a₂ : A} → (eq : a₁ ≡ a₂) → {z₁ z₂ : Z} → {t₁ t₂ : Z → StStream A Z} → StStream≈ (t₁ z₁) (t₂ z₂) → StStreamWEq A Z (cons (just a₁ , z₁) t₁) (cons (just a₂ , z₂) t₂)
    eqskipnil : {z₁ : Z} → {t₁ : Z → StStream A Z} → StStream≈ (t₁ z₁) (mkStream nil) → StStreamWEq A Z (cons (nothing , z₁) t₁) nil
    eqnilskip : {z₂ : Z} → {t₂ : Z → StStream A Z} → StStream≈ (mkStream nil) (t₂ z₂) → StStreamWEq A Z nil (cons (nothing , z₂) t₂)
    eqskipcons : {a₂ : A} → {z₁ z₂ : Z} → {t₁ t₂ : Z → StStream A Z} → StStreamWEq A Z (StStream.stream (t₁ z₁)) (cons (just a₂ , z₂) t₂) → StStreamWEq A Z (cons (nothing , z₁) t₁) (cons (just a₂ , z₂) t₂)
    eqconsskip : {a₁ : A} → {z₁ z₂ : Z} → {t₁ t₂ : Z → StStream A Z} → StStreamWEq A Z (cons (just a₁ , z₁) t₁) (StStream.stream (t₂ z₂)) → StStreamWEq A Z (cons (just a₁ , z₁) t₁) (cons (nothing , z₂) t₂)

  refl≈ : ∀ {A Z : Set} {st : StStream A Z} → StStream≈ st st
  StStream≈.stream (refl≈ {st = st}) = refl≈' (StStream.stream st)

  refl≈' : ∀ {A Z : Set} (st : StStreamF A Z) → StStreamWEq A Z st st
  refl≈' nil = eqnil
  refl≈' (cons (just x , z) tl) = eqcons refl refl≈
  refl≈' (cons (nothing , z) tl) = eqskip refl≈

  sym≈ : ∀ {A Z : Set} {st st' : StStream A Z} → StStream≈ st st' → StStream≈ st' st
  StStream≈.stream (sym≈ {st = st} {st' = st'} eq) = sym≈' (StStream≈.stream eq)
  
  sym≈' : ∀ {A Z : Set} {st st' : StStreamF A Z} → StStreamWEq A Z st st' → StStreamWEq A Z st' st
  sym≈' eqnil = eqnil
  sym≈' (eqskip eq) = eqskip (sym≈ eq)
  sym≈' (eqcons refl eq) = eqcons refl (sym≈ eq)
  sym≈' (eqskipnil eq) = eqnilskip (sym≈ eq)
  sym≈' (eqnilskip eq) = eqskipnil (sym≈ eq)
  sym≈' (eqskipcons eq) = eqconsskip (sym≈' eq)
  sym≈' (eqconsskip eq) = eqskipcons (sym≈' eq)


  trans≈ : ∀ {A Z : Set} {st₁ st₂ st₃ : StStream A Z} → (eq₁ : StStream≈ st₁ st₂) → (eq₂ : StStream≈ st₂ st₃) → StStream≈ st₁ st₃
  StStream≈.stream (trans≈ eq₁ eq₂) = trans≈' (StStream≈.stream eq₁) (StStream≈.stream eq₂)

  trans≈' : ∀ {A Z : Set} {st₁ st₂ st₃ : StStreamF A Z} → (eq₁ : StStreamWEq A Z st₁ st₂) → (eq₂ : StStreamWEq A Z st₂ st₃) → StStreamWEq A Z st₁ st₃
  trans≈' eqnil eqnil = eqnil
  trans≈' eqnil (eqnilskip eq₂) = eqnilskip (trans≈ (mkStream≈ eqnil) eq₂)
  trans≈' (eqskip eq₁) (eqskip eq₂) = eqskip (trans≈ eq₁ eq₂)
  trans≈' (eqskip eq₁) (eqskipnil eq₂) = eqskipnil (trans≈ eq₁ eq₂)
  trans≈' (eqskip {z₁ = z₁} {z₂ = z₂} {t₁ = t₁} {t₂ = t₂} eq₁) (eqskipcons {a₂ = a₂} {z₂ = z₃} {t₂ = t₃} eq₂) = eqskipcons (eqskipeqskipconsLemma  a₂ z₁ z₂ z₃ t₁ t₂ t₃ (StStream≈.stream eq₁) eq₂)
  trans≈' (eqcons refl eq₁) (eqcons refl eq₂) = eqcons refl (trans≈ eq₁ eq₂)
  trans≈' (eqcons eq eq₁) (eqconsskip eq₂) = eqconsskip (trans≈' (eqcons eq eq₁) eq₂)
  trans≈' (eqskipnil eq₁) eqnil = eqskipnil (trans≈ eq₁ (mkStream≈ eqnil))
  trans≈' (eqskipnil eq₁) (eqnilskip eq₂) = eqskip (trans≈ eq₁ eq₂)
  trans≈' (eqnilskip eq₁) (eqskip eq₂) = eqnilskip (trans≈ eq₁ eq₂)
  trans≈' (eqnilskip eq₁) (eqskipnil eq₂) = eqnil
  trans≈' (eqnilskip eq₁) (eqskipcons {a₂ = a₂} {z₁ = z₂} {z₂ = z₃} {t₁ = t₂} {t₂ = t₃} eq₂) = ⊥-elim (eqnilskipeqskipconsLemma a₂ z₂ z₃ t₂ t₃ (StStream≈.stream eq₁) eq₂)
  trans≈' (eqskipcons eq₁) (eqcons eq eq₂) = eqskipcons (trans≈' eq₁ (eqcons eq eq₂))
  trans≈' (eqskipcons eq₁) (eqconsskip eq₂) = eqskip (mkStream≈ (trans≈' eq₁ eq₂))
  trans≈' (eqconsskip {a₁ = a₁} {z₁ = z₁} {t₁ = t₁} eq₁) (eqskip {z₁ = z₂} {z₂ = z₃} {t₁ = t₂} {t₂ = t₃} eq₂) = eqconsskip (eqconsskipeqskipLemma a₁ z₁ z₂ z₃ t₁ t₂ t₃ eq₁ (StStream≈.stream eq₂))
  trans≈' (eqconsskip {a₁ = a₁} {z₁ = z₁} {z₂ = z₂} {t₁ = t₁} {t₂ = t₂} eq₁) (eqskipnil eq₂) = ⊥-elim (eqconsskipeqskipnilLemma a₁ z₁ z₂ t₁ t₂ eq₁ (StStream≈.stream eq₂))
  trans≈' (eqconsskip eq₁) (eqskipcons eq₂) = trans≈' eq₁ eq₂

  eqconsskipeqskipnilLemma : ∀ {A Z : Set} (a₁ : A) (z₁ z₂ : Z) (t₁ t₂ : Z → StStream A Z) → (eq₁ : StStreamWEq A Z (cons (just a₁ , z₁) t₁) (StStream.stream (t₂ z₂))) → (eq₂ : StStreamWEq A Z (StStream.stream (t₂ z₂)) nil) → ⊥
  eqconsskipeqskipnilLemma a₁ z₁ z₂ t₁ t₂ eq₁ eq₂ with StStream.stream (t₂ z₂)
  eqconsskipeqskipnilLemma a₁ z₁ z₂ t₁ t₂ (eqcons eq eq₁) () | .(cons (just _ , _) _)
  eqconsskipeqskipnilLemma a₁ z₁ z₂ t₁ t₂ (eqconsskip {z₂ = z₂'} {t₂ = t₂'} eq₁) (eqskipnil eq₂) | .(cons (nothing , _) _) = eqconsskipeqskipnilLemma a₁ z₁ z₂' t₁ t₂' eq₁ (StStream≈.stream eq₂)

  eqnilskipeqskipconsLemma : ∀ {A Z : Set} (a₂ : A) (z₂ z₃ : Z) (t₂ t₃ : Z → StStream A Z) → (eq₁ :  StStreamWEq A Z nil (StStream.stream (t₂ z₂))) → (eq₂ : StStreamWEq A Z (StStream.stream (t₂ z₂))
      (cons (just a₂ , z₃) t₃))  → ⊥
  eqnilskipeqskipconsLemma a₂ z₂ z₃ t₂ t₃ eq₁ eq₂ with StStream.stream (t₂ z₂)
  eqnilskipeqskipconsLemma a₂ z₂ z₃ t₂ t₃ eqnil () | .nil
  eqnilskipeqskipconsLemma a₂ z₂ z₃ t₂ t₃ (eqnilskip {z₂ = z₂'} {t₂ = t₂'} eq₁) (eqskipcons eq₂) | .(cons (nothing , _) _) = eqnilskipeqskipconsLemma a₂ z₂' z₃ t₂' t₃ (StStream≈.stream eq₁) eq₂

  eqconsskipeqskipLemma : ∀ {A Z : Set} (a₁ : A) (z₁ z₂ z₃ : Z) (t₁ t₂ t₃ : Z → StStream A Z) →
                            (eq₁ : StStreamWEq A Z (cons (just a₁ , z₁) t₁) (StStream.stream (t₂ z₂))) →
                            (eq₂ : StStreamWEq A Z (StStream.stream (t₂ z₂)) (StStream.stream (t₃ z₃))) →
                            StStreamWEq A Z (cons (just a₁ , z₁) t₁) (StStream.stream (t₃ z₃))
  eqconsskipeqskipLemma a₁ z₁ z₂ z₃ t₁ t₂ t₃ eq₁ eq₂ with StStream.stream (t₂ z₂) | StStream.stream (t₃ z₃)
  eqconsskipeqskipLemma a₁ z₁ z₂ z₃ t₁ t₂ t₃ (eqcons refl eq₁) (eqcons refl eq₂) | .(cons (just _ , _) _) | .(cons (just _ , _) _) =
    eqcons refl (trans≈ eq₁ eq₂)
  eqconsskipeqskipLemma a₁ z₁ z₂ z₃ t₁ t₂ t₃ (eqcons refl eq₁) (eqconsskip {z₁ = z₂'} {z₂ = z₃'} {t₁ = t₂'} {t₂ = t₃'} eq₂) | .(cons (just _ , _) _) | .(cons (nothing , _) _) =
    eqconsskip (eqconsskipeqskipLemma a₁ z₁ z₂' z₃' t₁ ((λ z → mkStream (cons (just a₁ , z) t₂'))) t₃' (eqcons refl eq₁) eq₂)
  eqconsskipeqskipLemma a₁ z₁ z₂ z₃ t₁ t₂ t₃ (eqconsskip {z₂ = z₂'} {t₂ = t₂'} eq₁) (eqskip {z₂ = z₃'} {t₂ = t₃'} eq₂) | .(cons (nothing , _) _) | .(cons (nothing , _) _) =
    eqconsskip (eqconsskipeqskipLemma a₁ z₁ z₂' z₃' t₁ t₂' t₃' eq₁ (StStream≈.stream eq₂))
  eqconsskipeqskipLemma a₁ z₁ z₂ z₃ t₁ t₂ t₃ (eqconsskip {z₂ = z₂'} {t₂ = t₂'} eq₁) (eqskipnil eq₂) | .(cons (nothing , _) _) | .nil =
    eqconsskipeqskipLemma a₁ z₁ z₂' z₃ t₁ t₂' (λ z → mkStream nil) eq₁ (StStream≈.stream eq₂)
  eqconsskipeqskipLemma a₁ z₁ z₂ z₃ t₁ t₂ t₃ (eqconsskip {z₂ = z₂'} {t₂ = t₂'} eq₁) (eqskipcons {a₂ = a₂} {z₂ = z₃'} {t₂ = t₃'} eq₂) | .(cons (nothing , _) _) | .(cons (just _ , _) _) =
    eqconsskipeqskipLemma a₁ z₁ z₂' z₃' t₁ t₂' (λ z → mkStream (cons (just a₂ , z) t₃')) eq₁ eq₂

  eqskipeqskipconsLemma : ∀ {A Z : Set} (a₂ : A) (z₁ z₂ z₃ : Z) (t₁ t₂ t₃ : Z → StStream A Z) →
                            (eq₁ : StStreamWEq A Z (StStream.stream (t₁ z₁)) (StStream.stream (t₂ z₂))) →
                            (eq₂ : StStreamWEq A Z (StStream.stream (t₂ z₂)) (cons (just a₂ , z₃) t₃)) →
                            StStreamWEq A Z (StStream.stream (t₁ z₁)) (cons (just a₂ , z₃) t₃)
  eqskipeqskipconsLemma a₂ z₁ z₂ z₃ t₁ t₂ t₃ eq₁ eq₂ with StStream.stream (t₁ z₁) | StStream.stream (t₂ z₂)
  eqskipeqskipconsLemma a₂ z₁ z₂ z₃ t₁ t₂ t₃ eqnil () | .nil | .nil
  eqskipeqskipconsLemma a₂ z₁ z₂ z₃ t₁ t₂ t₃ (eqskip {z₁ = z₁'} {z₂ = z₂'} {t₁ = t₁'} {t₂ = t₂'} eq₁) (eqskipcons eq₂) | .(cons (nothing , _) _) | .(cons (nothing , _) _) =
    eqskipcons (eqskipeqskipconsLemma a₂ z₁' z₂' z₃ t₁' t₂' t₃ (StStream≈.stream eq₁) eq₂)
  eqskipeqskipconsLemma a₂ z₁ z₂ z₃ t₁ t₂ t₃ (eqcons refl eq₁) (eqcons refl eq₂) | .(cons (just _ , _) _) | .(cons (just _ , _) _) =
    eqcons refl (trans≈ eq₁ eq₂)
  eqskipeqskipconsLemma a₂ z₁ z₂ z₃ t₁ t₂ t₃ (eqskipnil eq₁) () | .(cons (nothing , _) _) | .nil
  eqskipeqskipconsLemma a₂ z₁ z₂ z₃ t₁ t₂ t₃ (eqnilskip eq₁) (eqskipcons {z₁ = z₂'} {t₁ = t₂'} eq₂) | .nil | .(cons (nothing , _) _) =
    eqskipeqskipconsLemma a₂ z₁ z₂' z₃ (λ z → mkStream nil) t₂' t₃ (StStream≈.stream eq₁) eq₂
  eqskipeqskipconsLemma a₂ z₁ z₂ z₃ t₁ t₂ t₃ (eqskipcons {z₁ = z₁'} {z₂ = z₂'} {t₁ = t₁'} {t₂ = t₂'} eq₁) (eqcons refl eq₂) | .(cons (nothing , _) _) | .(cons (just _ , _) _) =
    eqskipcons (eqskipeqskipconsLemma a₂ z₁' z₂' z₃ t₁' (λ z → mkStream (cons (just a₂ , z) t₂')) t₃ eq₁ (eqcons refl eq₂))
  eqskipeqskipconsLemma a₂ z₁ z₂ z₃ t₁ t₂ t₃ (eqconsskip {a₁ = a₁} {z₁ = z₁'} {z₂ = z₂'} {t₁ = t₁'} {t₂ = t₂'} eq₁) (eqskipcons {t₂ = t₃'} eq₂) | .(cons (just _ , _) _) | .(cons (nothing , _) _) =
    eqskipeqskipconsLemma a₂ z₁' z₂' z₃ (λ z → mkStream (cons (just a₁ , z) t₁')) t₂' t₃' eq₁ eq₂

module _ where 
  open import Relation.Binary
  setoidStStream≈ : ∀ (A Z : Set) → Setoid _ _
  Setoid.Carrier (setoidStStream≈ A Z) = StStream A Z
  Setoid._≈_ (setoidStStream≈ A Z) = StStream≈
  IsEquivalence.refl (Setoid.isEquivalence (setoidStStream≈ A Z)) = refl≈
  IsEquivalence.sym (Setoid.isEquivalence (setoidStStream≈ A Z)) = sym≈
  IsEquivalence.trans (Setoid.isEquivalence (setoidStStream≈ A Z)) = trans≈

-- examples
module _ {A Z : Set} where
  open Stream₂ stream2Denot
  private
  
    skips : Z → StStream A Z
    StStream.stream (skips z) = cons (nothing , z) (λ z → skips z)

    skip≈nil : ∀ (z : Z) → StStream≈ (skips z) (mkStream nil)
    StStream≈.stream (skip≈nil z) = eqskipnil (skip≈nil z)
    
    appendskip : ∀ (z : Z) (tl : Z → StStream A Z) → StStream≈ (tl z) (mkStream (cons (nothing , z) tl))
    StStream≈.stream (appendskip z tl) = appendskip' z tl
      where
        appendskip' : ∀ (z : Z) (tl : Z → StStream A Z) → StStreamWEq A Z (StStream.stream (tl z)) (cons (nothing , z) tl)
        appendskip' z tl with StStream.stream (tl z) | inspect StStream.stream (tl z)
        appendskip' z tl | nil | [ prf ] = eqnilskip local
          where
            local : StStream≈ (mkStream nil) (tl z)
            StStream≈.stream local rewrite prf = eqnil
        appendskip' z tl | cons (just x , z') tl' | [ prf ] = eqconsskip local
          where
            local : StStreamWEq A Z (cons (just x , z') tl') (StStream.stream (tl z))
            local rewrite prf = eqcons refl refl≈
        appendskip' z tl | cons (nothing , z') tl' | [ prf ] = eqskip local
          where
            local : StStream≈ (tl' z') (tl z)
            StStream≈.stream local rewrite prf = appendskip' z' tl'

    singleton : ∀ (z : Z) (a : A) → StStream A Z 
    StStream.stream (singleton z a) = cons ((just a) , z) (λ _ → mkStream nil)

    mutual
      flatmapSing : ∀ (st : StStream A Z) → StStream≈ st (st |> flatmapSt (λ z a → singleton z a))
      StStream≈.stream (flatmapSing st) with StStream.stream st
      StStream≈.stream (flatmapSing st) | nil = eqnil
      StStream≈.stream (flatmapSing st) | cons (just x , z) tl = eqconsskip (eqcons refl (flatmapSing' z tl))
      StStream≈.stream (flatmapSing st) | cons (nothing , z) tl = eqskip (flatmapSing (tl z))

      flatmapSing' : ∀ (z : Z) (tl : Z → StStream A Z) → StStream≈ (tl z) (StStreamFunctions.flatmapInnerSt singleton z tl (mkStream nil))
      StStream≈.stream (flatmapSing' z tl) = local z tl
        where
          local : (z : Z) (tl : Z → StStream A Z) → StStreamWEq A Z (StStream.stream (tl z)) (cons (nothing , z) (λ z₁ → flatmapSt singleton (tl z₁)))
          local z tl with StStream.stream (tl z) | inspect StStream.stream (tl z) 
          local z tl | nil | [ prf ]  = eqnilskip local'
            where
              local' : StStream≈ (mkStream nil) (flatmapSt singleton (tl z))
              StStream≈.stream local' rewrite prf = eqnil
          local z tl | cons (just x , z') tl' | [ prf ]  = eqconsskip local'
            where
              local' : StStreamWEq A Z (cons (just x , z') tl') (StStream.stream (flatmapSt singleton (tl z)))
              local' rewrite prf = eqconsskip (eqcons refl (flatmapSing' z' tl'))
          local z tl | cons (nothing , z') tl' | [ prf ] = eqskip local'
            where
              local' : StStream≈ (tl' z') (flatmapSt singleton (tl z))
              StStream≈.stream local' rewrite prf = local z' tl'


-- Equations among streams
module _ where
  open Stream₂ stream2Denot

  module _ {A Z : Set} where 
    private
      step' : ∀ {upb : ℕ} (step : Z → Fin upb → A × Z) → Z × Z → Fin upb → (A × Z × Z)
      step' step (z₁ , z) i with step z i
      ... | a , zn = a , z₁ , zn
      
    initPull : ∀ (z : Z) (z₁ : Z) (upb : ℕ) (step : Z → Fin upb → A × Z) →
                 StStream≅ (pullArray z upb step |> initSt z₁) (pullArray (z₁ , z) upb (step' step))
    StStream≅.stream (initPull z z₁ upb step) with upb
    StStream≅.stream (initPull z z₁ upb step) | zero = eqnil
    StStream≅.stream (initPull z z₁ upb step) | suc n with step z zero
    StStream≅.stream (initPull z z₁ upb step) | suc n | a , z' = eqcons refl (initPull z' z₁ n (λ z' n' → step z' (suc n')))

  module _ {A Z : Set} (step : Z → Maybe A × Z) where 
    private
      step' : Z × Z → (Maybe A × Z × Z)
      step' (z₁ , z) with step z
      ... | a , zn = a , (z₁ , zn)

    initUnroll : ∀ (z : Z) (z₁ : Z) →
                    StStream≅ (unroll step z |> initSt z₁) (unroll step' (z₁ , z))
    StStream≅.stream (initUnroll z z₁) with step z
    StStream≅.stream (initUnroll z z₁) | just a , z' = eqcons refl (initUnroll z' z₁)
    StStream≅.stream (initUnroll z z₁) | nothing , z' = eqskip (initUnroll z' z₁)


  module _ {A B Z : Set} (f : Z → A → Maybe B × Z)  where
    private
      f' :  Z × Z → A → Maybe B × Z × Z
      f' (z , z₁) a with f z₁ a
      ... | b , z₁' = b , (z , z₁')
    
    initMap : ∀ (z : Z) (st : StStream A Z) → StStream≅ (st |> mapFilterSt f |> initSt z) (st |> initSt z |> mapFilterSt f')
    StStream≅.stream (initMap z st) with (StStream.stream st)
    StStream≅.stream (initMap z st) | nil = eqnil
    StStream≅.stream (initMap z st) | cons (just a , z₁) tl with f z₁ a
    StStream≅.stream (initMap z st) | cons (just a , z₁) tl | just b , z₁' = eqcons refl (initMap z (tl z₁'))
    StStream≅.stream (initMap z st) | cons (just a , z₁) tl | nothing , z₁' = eqskip (initMap z (tl z₁'))
    StStream≅.stream (initMap z st) | cons (nothing , z₁) tl = eqskip (initMap z (tl z₁))

  module _ {A B Z : Set} (f : Z → A → Maybe B × Z)  where
    private
      f' : Z × Z → A → Maybe B × Z × Z
      f'  (z , z₁) a with f z₁ a
      ... | b , z₁' = b , z , z₁'

    abstractMap : ∀ (st : StStream A (Z × Z)) → StStream≅ (st |> abstractSt |> mapFilterSt f) (st |> mapFilterSt f' |> abstractSt)
    StStream≅.stream (abstractMap st) with (StStream.stream st)
    StStream≅.stream (abstractMap st) | nil = eqnil
    StStream≅.stream (abstractMap st) | cons (just a , z , z₁) tl with f z₁ a
    StStream≅.stream (abstractMap st) | cons (just a , z , z₁) tl | just b , z₁' = eqcons refl (abstractMap (tl (z , z₁')))
    StStream≅.stream (abstractMap st) | cons (just a , z , z₁) tl | nothing , z₁' = eqskip (abstractMap (tl (z , z₁')))
    StStream≅.stream (abstractMap st) | cons (nothing , z , z₁) tl = eqskip (abstractMap (tl (z , z₁) ))

  module _ {A Z : Set} (pred : Z → Bool)  where
    private
      pred' : Z × Z → Bool
      pred' (z , z₁) = pred z₁

    initGuard : ∀ (z : Z) (st : StStream A Z) → StStream≅ (st |> guard pred |> initSt z) (st |> initSt z |> guard pred')
    StStream≅.stream (initGuard z st) with (StStream.stream st)
    StStream≅.stream (initGuard z st) | nil = eqnil
    StStream≅.stream (initGuard z st) | cons (just a , z₁) tl with pred z₁
    StStream≅.stream (initGuard z st) | cons (just a , z₁) tl | false = eqnil
    StStream≅.stream (initGuard z st) | cons (just a , z₁) tl | true = eqcons refl (initGuard z (tl z₁))
    StStream≅.stream (initGuard z st) | cons (nothing , z₁) tl with pred z₁
    StStream≅.stream (initGuard z st) | cons (nothing , z₁) tl | false = eqnil
    StStream≅.stream (initGuard z st) | cons (nothing , z₁) tl | true = eqskip (initGuard z (tl z₁))

  module _ {A Z : Set} (pred : Z → Bool)  where
    private
      pred' : Z × Z → Bool
      pred' (z , z₁) = pred z₁
    
    abstractGuard : ∀ (st : StStream A (Z × Z)) → StStream≅ (st |> abstractSt |> guard pred) (st |> guard pred' |> abstractSt)
    StStream≅.stream (abstractGuard st) with (StStream.stream st)
    StStream≅.stream (abstractGuard st) | nil = eqnil
    StStream≅.stream (abstractGuard st) | cons (just a , z , z₁) tl with pred z₁
    StStream≅.stream (abstractGuard st) | cons (just a , z , z₁) tl | false = eqnil
    StStream≅.stream (abstractGuard st) | cons (just a , z , z₁) tl | true = eqcons refl (abstractGuard (tl (z , z₁)))
    StStream≅.stream (abstractGuard st) | cons (nothing , z , z₁) tl with pred z₁
    StStream≅.stream (abstractGuard st) | cons (nothing , z , z₁) tl | false = eqnil
    StStream≅.stream (abstractGuard st) | cons (nothing , z , z₁) tl | true = eqskip (abstractGuard (tl (z , z₁)))


  module _ {A B Z₁ Z₂ Z : Set} where
    private
      adjustState : StStream (A × B) (Z × (Z₁ × Z₂)) → StStream (A × B) ((Z × Z₁) × Z₂)
      adjustState st = st |> adjust ((λ { (z , (z₁ , z₂)) → ((z , z₁) , z₂) }) , (λ { ((z , z₁) , z₂) → (z , (z₁ , z₂)) }))

    initZip : ∀ (st₁ : StStream A Z₁) (st₂ : StStream B Z₂) (z : Z) → StStream≅ (zipSt (initSt z st₁) st₂) (zipSt st₁ st₂ |> initSt z |> adjustState) 
    StStream≅.stream (initZip st₁ st₂ z) with (StStream.stream st₁) 
    StStream≅.stream (initZip st₁ st₂ z) | nil = eqnil
    StStream≅.stream (initZip st₁ st₂ z) | cons (just a , z₁) tl₁ with (StStream.stream st₂)
    StStream≅.stream (initZip st₁ st₂ z) | cons (just a , z₁) tl₁ | nil = eqnil
    StStream≅.stream (initZip st₁ st₂ z) | cons (just a , z₁) tl₁ | cons (just b , z₂) tl₂ = eqcons refl (initZip (tl₁ z₁) (tl₂ z₂) z)
    StStream≅.stream (initZip st₁ st₂ z) | cons (just a , z₁) tl₁ | cons (nothing , z₂) tl₂ = eqskip (initZip st₁ (tl₂ z₂) z)
    StStream≅.stream (initZip st₁ st₂ z) | cons (nothing , z₁) tl₁ with (StStream.stream st₂)
    StStream≅.stream (initZip st₁ st₂ z) | cons (nothing , z₁) tl₁ | nil = eqnil
    StStream≅.stream (initZip st₁ st₂ z) | cons (nothing , z₁) tl₁ | cons (just b , z₂) tl₂ = eqskip (initZip (tl₁ z₁) st₂ z)
    StStream≅.stream (initZip st₁ st₂ z) | cons (nothing , z₁) tl₁ | cons (nothing , z₂) tl₂ = eqskip (initZip (tl₁ z₁) (tl₂ z₂) z)

  module _ {A B Z₁ Z₂ Z : Set} where
    private
      adjustState : StStream (A × B) ((Z × Z₁) × Z₂) → StStream (A × B) (Z × (Z₁ × Z₂))
      adjustState st = st |> adjust ((λ { ((z , z₁) , z₂) → (z , (z₁ , z₂)) }), (λ { (z , (z₁ , z₂)) → ((z , z₁) , z₂) }))

    abstractZip : ∀ (st₁ : StStream A (Z × Z₁)) (st₂ : StStream B Z₂) → StStream≅ (zipSt (abstractSt st₁) st₂) (zipSt st₁ st₂ |> adjustState |> abstractSt)
    StStream≅.stream (abstractZip st₁ st₂) with StStream.stream st₁
    StStream≅.stream (abstractZip st₁ st₂) | nil = eqnil
    StStream≅.stream (abstractZip st₁ st₂) | cons (just a , z₁) tl₁ with StStream.stream st₂
    StStream≅.stream (abstractZip st₁ st₂) | cons (just a , z₁) tl₁ | nil = eqnil
    StStream≅.stream (abstractZip st₁ st₂) | cons (just a , z₁) tl₁ | cons (just b , z₂) tl₂ = eqcons refl (abstractZip (tl₁ z₁) (tl₂ z₂))
    StStream≅.stream (abstractZip st₁ st₂) | cons (just a , z₁) tl₁ | cons (nothing , z₂) tl₂ = eqskip (abstractZip st₁ (tl₂ z₂))
    StStream≅.stream (abstractZip st₁ st₂) | cons (nothing , z₁) tl₁ with StStream.stream st₂
    StStream≅.stream (abstractZip st₁ st₂) | cons (nothing , z₁) tl₁ | nil = eqnil
    StStream≅.stream (abstractZip st₁ st₂) | cons (nothing , z₁) tl₁ | cons (just b , z₂) tl₂ = eqskip (abstractZip (tl₁ z₁) st₂)
    StStream≅.stream (abstractZip st₁ st₂) | cons (nothing , z₁) tl₁ | cons (nothing , z₂) tl₂ = eqskip (abstractZip (tl₁ z₁) (tl₂ z₂))

  module _ {A B Z₁ Z : Set} (f : Z₁ → A → StStream B Z₁) where
    private
       f' : Z × Z₁ → A → StStream B (Z × Z₁)
       f' (z , z₁) a = let sti = f z₁ a in sti |> initSt z

    mutual 
      initFlatmap : ∀ (st : StStream A Z₁) (z : Z) → StStream≅ (st |> flatmapSt f |> initSt z) (st |> initSt z |> flatmapSt f')
      StStream≅.stream (initFlatmap st z) with StStream.stream st
      StStream≅.stream (initFlatmap st z) | nil = eqnil
      StStream≅.stream (initFlatmap st z) | cons (just a , z₁) tl = eqskip (initFlatmap' z₁ z tl (f z₁ a))
      StStream≅.stream (initFlatmap st z) | cons (nothing , z₁) tl = eqskip (initFlatmap (tl z₁) z)

      initFlatmap' : ∀ (z₁ : Z₁) (z : Z) (tl : Z₁ → StStream A Z₁) → (st : StStream B Z₁) →
                          StStream≅ (initSt z (StStreamFunctions.flatmapInnerSt f z₁ tl st))
                                    (StStreamFunctions.flatmapInnerSt f' (z , z₁) (λ { (z , z₁) → initSt z (tl z₁) }) (initSt z st))
      StStream≅.stream (initFlatmap' z₁ z tl st) with StStream.stream st
      StStream≅.stream (initFlatmap' z₁ z tl st) | nil = eqskip (initFlatmap (tl z₁) z)
      StStream≅.stream (initFlatmap' z₁ z tl st) | cons (just b , z') tl' = eqcons refl (initFlatmap' z' z tl (tl' z'))
      StStream≅.stream (initFlatmap' z₁ z tl st) | cons (nothing , z') tl' = eqskip (initFlatmap' z' z tl (tl' z'))

  module _ {A B Z₁ Z : Set} (f : Z₁ → A → StStream B Z₁) where
    private
      f' : Z × Z₁ → A → StStream B (Z × Z₁)
      f' (z , z₁) a = let sti = f z₁ a in sti |> initSt z

    mutual
      abstractFlatmap : ∀ (st : StStream A (Z × Z₁)) → StStream≅ (st |> abstractSt |> flatmapSt f) (st |> flatmapSt f' |> abstractSt)
      StStream≅.stream (abstractFlatmap st) with StStream.stream st
      StStream≅.stream (abstractFlatmap st) | nil = eqnil
      StStream≅.stream (abstractFlatmap st) | cons (just a , z , z₁) tl = eqskip (abstractFlatmap' z₁ z tl (f z₁ a))
      StStream≅.stream (abstractFlatmap st) | cons (nothing , z , z₁) tl = eqskip (abstractFlatmap (tl (z , z₁)))

      abstractFlatmap' : ∀ (z₁ : Z₁) (z : Z) (tl : (Z × Z₁) → StStream A (Z × Z₁)) (st : StStream B Z₁) →
                      StStream≅ (StStreamFunctions.flatmapInnerSt f z₁ (λ z₂ → abstractSt (tl (z , z₂))) st)
                                (abstractSt (StStreamFunctions.flatmapInnerSt f' (z , z₁) tl (initSt z st)))
      StStream≅.stream (abstractFlatmap' z₁ z tl st) with StStream.stream st
      StStream≅.stream (abstractFlatmap' z₁ z tl st) | nil = eqskip (abstractFlatmap (tl (z , z₁)))
      StStream≅.stream (abstractFlatmap' z₁ z tl st) | cons (just b , z') tl' = eqcons refl (abstractFlatmap' z' z tl (tl' z'))
      StStream≅.stream (abstractFlatmap' z₁ z tl st) | cons (nothing , z') tl' = eqskip (abstractFlatmap' z' z tl (tl' z'))

  module _ {A Z₁ Z₂ Z : Set} where

    initAbstract : ∀ (st : StStream A (Z₁ × Z₂)) (z : Z) → StStream≅ (st |> abstractSt |> initSt z)
                                                                     (st |> initSt z |> adjust ((λ { (z , (z₁ , z₂)) → (z₁ , (z , z₂)) }) ,
                                                                                                (λ { (z₁ , (z , z₂)) → (z , (z₁ , z₂)) })) |> abstractSt)
    StStream≅.stream (initAbstract st z) with StStream.stream st
    StStream≅.stream (initAbstract st z) | nil = eqnil
    StStream≅.stream (initAbstract st z) | cons (just a , (z₁ , z₂)) tl = eqcons refl (initAbstract (tl (z₁ , z₂)) z)
    StStream≅.stream (initAbstract st z) | cons (nothing , (z₁ , z₂)) tl = eqskip (initAbstract (tl (z₁ , z₂)) z)

  module _ {A Z₁ Z : Set} where

    abstractInit : ∀ (st : StStream A Z₁) (z : Z) → StStream≅ (st |> initSt z |> abstractSt) st
    StStream≅.stream (abstractInit st z) with StStream.stream st
    StStream≅.stream (abstractInit st z) | nil = eqnil
    StStream≅.stream (abstractInit st z) | cons (just a , z₁) tl = eqcons refl (abstractInit (tl z₁) z)
    StStream≅.stream (abstractInit st z) | cons (nothing , z₁) tl = eqskip (abstractInit (tl z₁) z)

  module _ {A Z : Set} where
    private
      step' : ((Σ ℕ (λ n → (Z → Fin n → A × Z) × Bool)) × Z) → Maybe A × ((Σ ℕ (λ n → (Z → Fin n → A × Z) × Bool)) × Z)
      step' ((n , (step , b)) , z) with n
      step' ((n , (step , b)) , z) | zero = nothing , (zero , (step , false)) , z 
      step' ((n , (step , b)) , z) | suc n' with step z zero
      step' ((n , (step , b)) , z) | suc n' | x , z' = (just x) , (n' , (λ z n → step z (suc n)) , true) , z'
    
    pullArrayUnroll : ∀ (z : Z) (upb : ℕ) (step : Z → Fin upb → A × Z) →
                        StStream≅ (pullArray z upb step) (unroll step' ((upb , step , true) , z) |> guard (λ { ((_ , (_ , b)) , _) → b }) |> abstractSt)
    StStream≅.stream (pullArrayUnroll z upb step) with upb
    StStream≅.stream (pullArrayUnroll z upb step) | zero = eqnil
    StStream≅.stream (pullArrayUnroll z upb step) | suc n with step z zero
    StStream≅.stream (pullArrayUnroll z upb step) | suc n | x , z' = eqcons refl (pullArrayUnroll z' n (λ z n → step z (suc n)))

  module _ {A B Z : Set} (step : Z → Maybe A × Z) (f : Z → A → Maybe B × Z) where
    private
      step' : Z → Maybe B × Z
      step' z with step z
      ... | just a , z' = f z' a
      ... | nothing , z' = nothing , z'

    mapUnroll : ∀ (z : Z) → StStream≅ (unroll step z |> mapFilterSt f) (unroll step' z)
    StStream≅.stream (mapUnroll z) with step z
    StStream≅.stream (mapUnroll z) | just a , z' with f z' a
    StStream≅.stream (mapUnroll z) | just a , z' | just b , z'' = eqcons refl (mapUnroll z'')
    StStream≅.stream (mapUnroll z) | just a , z' | nothing , z'' = eqskip (mapUnroll z'')
    StStream≅.stream (mapUnroll z) | nothing , z' = eqskip (mapUnroll z')

  module _ {A Z : Set} (pred₁ : Z → Bool) (pred₂ : Z → Bool) where

    guardGuard : ∀ (st : StStream A Z) → StStream≅ (st |> guard pred₁ |> guard pred₂) (st |> guard (λ x → pred₁ x ∧ pred₂ x))
    StStream≅.stream (guardGuard st) with StStream.stream st
    StStream≅.stream (guardGuard st) | nil = eqnil
    StStream≅.stream (guardGuard st) | cons (just a , z) tl with pred₁ z 
    StStream≅.stream (guardGuard st) | cons (just a , z) tl | false = eqnil
    StStream≅.stream (guardGuard st) | cons (just a , z) tl | true with pred₂ z
    StStream≅.stream (guardGuard st) | cons (just a , z) tl | true | false = eqnil
    StStream≅.stream (guardGuard st) | cons (just a , z) tl | true | true = eqcons refl (guardGuard (tl z))
    StStream≅.stream (guardGuard st) | cons (nothing , z) tl with pred₁ z
    StStream≅.stream (guardGuard st) | cons (nothing , z) tl | false = eqnil
    StStream≅.stream (guardGuard st) | cons (nothing , z) tl | true with pred₂ z 
    StStream≅.stream (guardGuard st) | cons (nothing , z) tl | true | false = eqnil
    StStream≅.stream (guardGuard st) | cons (nothing , z) tl | true | true = eqskip (guardGuard (tl z))

  module _ {A B Z : Set} (pred : Z → Bool) (f : Z → A → Maybe B × Z) (side₁ : (a : A) (z : Z) → let (b , z') = f z a in pred z' ≡ pred z) where

    mapGuard : ∀ (st : StStream A Z) → StStream≅ (st |> guard pred |> mapFilterSt f) (st |> mapFilterSt f |> guard pred)
    StStream≅.stream (mapGuard st) with StStream.stream st
    StStream≅.stream (mapGuard st) | nil = eqnil
    StStream≅.stream (mapGuard st) | cons (just a , z) tl rewrite (side₁ a z) with pred z
    StStream≅.stream (mapGuard st) | cons (just a , z) tl | false = eqnil
    StStream≅.stream (mapGuard st) | cons (just a , z) tl | true with f z a
    StStream≅.stream (mapGuard st) | cons (just a , z) tl | true | just b , z' = eqcons refl (mapGuard (tl z'))
    StStream≅.stream (mapGuard st) | cons (just a , z) tl | true | nothing , z' = eqskip (mapGuard (tl z'))
    StStream≅.stream (mapGuard st) | cons (nothing , z) tl with pred z
    StStream≅.stream (mapGuard st) | cons (nothing , z) tl | false = eqnil
    StStream≅.stream (mapGuard st) | cons (nothing , z) tl | true = eqskip (mapGuard (tl z))

  module _ {A B Z : Set} (pred : Z → Bool) (f : Z → A → Maybe B × Z) (side₁ : (a : A) (z : Z) → pred z ≡ false → let (b , z') = f z a in pred z' ≡ false) 
                                                                     (side₂ : (a : A) (z : Z) → pred z ≡ true → let (b , z') = f z a in pred z' ≡ false →
                                                                                                b ≡ nothing × ((a : A) (z : Z) → pred z ≡ true → let (b , z') = f z a in b ≡ nothing)) where
    private      
      lemma : ∀ (side : (a : A) (z : Z) → pred z ≡ true → let (b , z') = f z a in b ≡ nothing) (st : StStream A Z) → StStream≈ (st |> guard pred |> mapFilterSt f) (mkStream nil)
      StStream≈.stream (lemma side st) with StStream.stream st
      StStream≈.stream (lemma side st) | nil = eqnil
      StStream≈.stream (lemma side st) | cons (just a , z) tl with pred z | inspect pred z
      StStream≈.stream (lemma side st) | cons (just a , z) tl | false | [ eqpredz ] = eqnil
      StStream≈.stream (lemma side st) | cons (just a , z) tl | true  | [ eqpredz ] with f z a | inspect (λ { (z , a) → f z a}) (z , a)
      StStream≈.stream (lemma side st) | cons (just a , z) tl | true  | [ eqpredz ] | (b , z') | [ eqfza ] with side a z eqpredz
      StStream≈.stream (lemma side st) | cons (just a , z) tl | true  | [ eqpredz ] | (b , z') | [ eqfza ] | prf rewrite eqfza rewrite prf = eqskipnil (lemma side (tl z'))
      StStream≈.stream (lemma side st) | cons (nothing , z) tl with pred z
      StStream≈.stream (lemma side st) | cons (nothing , z) tl | false = eqnil
      StStream≈.stream (lemma side st) | cons (nothing , z) tl | true = eqskipnil (lemma side (tl z))

    mapGuard' : ∀ (st : StStream A Z) → StStream≈ (st |> guard pred |> mapFilterSt f) (st |> mapFilterSt f |> guard pred)
    StStream≈.stream (mapGuard' st) with StStream.stream st
    StStream≈.stream (mapGuard' st) | nil = eqnil
    StStream≈.stream (mapGuard' st) | cons (just a , z) tl with pred z | inspect pred z
    StStream≈.stream (mapGuard' st) | cons (just a , z) tl | false | [ eqpredz ] with f z a | inspect (λ { (z , a) → f z a}) (z , a)
    StStream≈.stream (mapGuard' st) | cons (just a , z) tl | false | [ eqpredz ] | (b , z') | [ eqfza ] with (side₁ a z eqpredz)
    StStream≈.stream (mapGuard' st) | cons (just a , z) tl | false | [ eqpredz ] | (b , z') | [ eqfza ] | prf rewrite eqfza rewrite prf = eqnil
    StStream≈.stream (mapGuard' st) | cons (just a , z) tl | true  | [ eqpredz ] with f z a | inspect (λ { (z , a) → f z a}) (z , a)
    StStream≈.stream (mapGuard' st) | cons (just a , z) tl | true  | [ eqpredz ] | (b , z') | [ eqfza ] with pred z' | inspect pred z'
    StStream≈.stream (mapGuard' st) | cons (just a , z) tl | true  | [ eqpredz ] | (b , z') | [ eqfza ] | false | [ eqpredz' ] with side₂ a z
    StStream≈.stream (mapGuard' st) | cons (just a , z) tl | true  | [ eqpredz ] | (b , z') | [ eqfza ] | false | [ eqpredz' ] | prf rewrite eqfza rewrite proj₁ (prf eqpredz eqpredz') = eqskipnil (lemma (proj₂ (prf eqpredz eqpredz')) (tl z'))
    StStream≈.stream (mapGuard' st) | cons (just a , z) tl | true  | [ eqpredz ] | just b , z'  | [ eqfza ] | true | [ eqpredz' ] = eqcons refl (mapGuard' (tl z'))
    StStream≈.stream (mapGuard' st) | cons (just a , z) tl | true  | [ eqpredz ] | nothing , z' | [ eqfza ] | true | [ eqpredz' ] = eqskip (mapGuard' (tl z'))
    StStream≈.stream (mapGuard' st) | cons (nothing , z) tl with pred z
    StStream≈.stream (mapGuard' st) | cons (nothing , z) tl | false = eqnil
    StStream≈.stream (mapGuard' st) | cons (nothing , z) tl | true = eqskip (mapGuard' (tl z))

  module _ {A B Z : Set} (pred : Z → Bool) (f : Z → A → StStream B Z) (side₁ : (a : A) (z : Z) → pred z ≡ true →
                                                                               (StStream.stream (f z a)) |> λ { nil → ⊤ ; (cons (_ , z') tl') → pred z' |> λ { false → ⊥ ; true → ⊤ } }) 
                                                                      (side₂ : (z : Z) (tl : Z → StStream B Z) → pred z ≡ true →
                                                                               (StStream.stream (tl z)) |> λ { nil → ⊤ ; (cons (_ , z') tl') → pred z' |> λ { false → ⊥ ; true → ⊤ } }) where
    mutual
      guardFlatmap : ∀ (st : StStream A Z) → StStream≅ (st |> guard pred |> flatmapSt f) (st |> flatmapSt f |> guard pred)
      StStream≅.stream (guardFlatmap st) with StStream.stream st
      StStream≅.stream (guardFlatmap st) | nil = eqnil
      StStream≅.stream (guardFlatmap st) | cons (just a , z) tl with pred z | inspect pred z
      StStream≅.stream (guardFlatmap st) | cons (just a , z) tl | false | [ eqpredz ] = eqnil
      StStream≅.stream (guardFlatmap st) | cons (just a , z) tl | true | [ eqpredz ] = eqskip (guardFlatmap' a z tl eqpredz)
      StStream≅.stream (guardFlatmap st) | cons (nothing , z) tl with pred z
      StStream≅.stream (guardFlatmap st) | cons (nothing , z) tl | false = eqnil
      StStream≅.stream (guardFlatmap st) | cons (nothing , z) tl | true = eqskip (guardFlatmap (tl z))

      guardFlatmap'' : ∀ (z : Z) (outertl : Z → StStream A Z) (tl : Z → StStream B Z) (prf : pred z ≡ true) →
                        StStream≅ (StStreamFunctions.flatmapInnerSt f z (λ z₁ → guard pred (outertl z₁)) (tl z))
                                  (guard pred (StStreamFunctions.flatmapInnerSt f z outertl (tl z)))
      StStream≅.stream (guardFlatmap'' z outertl tl prf) with side₂ z tl prf
      StStream≅.stream (guardFlatmap'' z outertl tl prf) | c with StStream.stream (tl z)
      StStream≅.stream (guardFlatmap'' z outertl tl prf) | c | nil rewrite prf = eqskip (guardFlatmap (outertl z))
      StStream≅.stream (guardFlatmap'' z outertl tl prf) | c | cons (just b , z') tl' with pred z' | inspect pred z'
      StStream≅.stream (guardFlatmap'' z outertl tl prf) | c | cons (just b , z') tl' | true | [ eqpredz ] = eqcons refl (guardFlatmap'' z' outertl tl' eqpredz)
      StStream≅.stream (guardFlatmap'' z outertl tl prf) | c | cons (nothing , z') tl' with pred z' | inspect pred z'
      StStream≅.stream (guardFlatmap'' z outertl tl prf) | c | cons (nothing , z') tl' | true | [ eqpredz ] = eqskip (guardFlatmap'' z' outertl tl' eqpredz)
      
      guardFlatmap' : ∀ (a : A) (z : Z) (outertl : Z → StStream A Z) (prf : pred z ≡ true) →
                        StStream≅ (StStreamFunctions.flatmapInnerSt f z (λ z₁ → guard pred (outertl z₁)) (f z a))
                                  (guard pred (StStreamFunctions.flatmapInnerSt f z outertl (f z a)))
      StStream≅.stream (guardFlatmap' a z outertl prf) with side₁ a z prf
      StStream≅.stream (guardFlatmap' a z outertl prf) | c with StStream.stream (f z a)
      StStream≅.stream (guardFlatmap' a z outertl prf) | c | nil rewrite prf = eqskip (guardFlatmap (outertl z))
      StStream≅.stream (guardFlatmap' a z outertl prf) | c | cons (just b , z') tl with pred z' | inspect pred z'
      StStream≅.stream (guardFlatmap' a z outertl prf) | c | cons (just b , z') tl | true | [ eqpredz ] = eqcons refl (guardFlatmap'' z' outertl tl eqpredz)
      StStream≅.stream (guardFlatmap' a z outertl prf) | c | cons (nothing , z') tl with pred z' | inspect pred z'
      StStream≅.stream (guardFlatmap' a z outertl prf) | c | cons (nothing , z') tl | true | [ eqpredz ] = eqskip (guardFlatmap'' z' outertl tl eqpredz)
      
  module _ {A B C Z : Set} (f₁ : Z → A → StStream B Z) (f₂ : Z → B → Maybe C × Z) where
    private
      f : Z → A → StStream C Z
      f z x = let sti = f₁ z x in mapFilterSt f₂ sti

    mutual
      mapFlatmap : ∀ (st : StStream A Z) → StStream≅ (st |> flatmapSt f₁ |> mapFilterSt f₂) (st |> flatmapSt f)
      StStream≅.stream (mapFlatmap st) with StStream.stream st
      StStream≅.stream (mapFlatmap st) | nil = eqnil
      StStream≅.stream (mapFlatmap st) | cons (just a , z) tl = eqskip (mapFlatmap' z tl (f₁ z a))
      StStream≅.stream (mapFlatmap st) | cons (nothing , z) tl = eqskip (mapFlatmap (tl z))

      mapFlatmap' : ∀ (z : Z) (tl : Z → StStream A Z) (st : StStream B Z) →
                      StStream≅ (mapFilterSt f₂ (StStreamFunctions.flatmapInnerSt f₁ z tl st))
                                (StStreamFunctions.flatmapInnerSt (λ z₁ x → mapFilterSt f₂ (f₁ z₁ x)) z tl (mapFilterSt f₂ st))
      StStream≅.stream (mapFlatmap' z tl st) with StStream.stream st
      StStream≅.stream (mapFlatmap' z tl st) | nil = eqskip (mapFlatmap (tl z))
      StStream≅.stream (mapFlatmap' z tl st) | cons (just b , z') tl' with f₂ z' b
      StStream≅.stream (mapFlatmap' z tl st) | cons (just b , z') tl' | just c , z'' = eqcons refl (mapFlatmap' z'' tl (tl' z''))
      StStream≅.stream (mapFlatmap' z tl st) | cons (just b , z') tl' | nothing , z'' = eqskip (mapFlatmap' z'' tl (tl' z''))
      StStream≅.stream (mapFlatmap' z tl st) | cons (nothing , z') tl' = eqskip (mapFlatmap' z' tl (tl' z'))

  module _ {A B C Z : Set} (f₁ : Z → A → StStream B Z) (f₂ : Z → B → StStream C Z) where
    private
      f : Z → A → StStream C Z
      f z x = let sti = f₁ z x in flatmapSt f₂ sti

    mutual
      flatmapFlatmap : ∀ (st : StStream A Z) → StStream≅ (st |> flatmapSt f₁ |> flatmapSt f₂) (st |> flatmapSt f)
      StStream≅.stream (flatmapFlatmap st) with StStream.stream st
      StStream≅.stream (flatmapFlatmap st) | nil = eqnil
      StStream≅.stream (flatmapFlatmap st) | cons (just a , z) tl = eqskip (flatmapFlatmap' z tl (f₁ z a))
      StStream≅.stream (flatmapFlatmap st) | cons (nothing , z) tl = eqskip (flatmapFlatmap (tl z))

      flatmapFlatmap' : ∀ (z : Z) (tl : Z → StStream A Z) (st : StStream B Z) →
                          StStream≅ (StStreamFunctions.flatmapSt f₂ (StStreamFunctions.flatmapInnerSt f₁ z tl st))
                                    (StStreamFunctions.flatmapInnerSt (λ z₁ x → StStreamFunctions.flatmapSt f₂ (f₁ z₁ x)) z tl (StStreamFunctions.flatmapSt f₂ st))
      StStream≅.stream (flatmapFlatmap' z tl st) with StStream.stream st
      StStream≅.stream (flatmapFlatmap' z tl st) | nil = eqskip (flatmapFlatmap (tl z))
      StStream≅.stream (flatmapFlatmap' z tl st) | cons (just b , z') tl' = eqskip (flatmapFlatmap'' z' tl tl' (f₂ z' b))
      StStream≅.stream (flatmapFlatmap' z tl st) | cons (nothing , z') tl' = eqskip (flatmapFlatmap' z' tl (tl' z'))

      flatmapFlatmap'' : ∀ (z' : Z) (tl : Z → StStream A Z) (tl' : Z → StStream B Z) (st : StStream C Z) →
                           StStream≅ (StStreamFunctions.flatmapInnerSt f₂ z' (λ z₁ → StStreamFunctions.flatmapInnerSt f₁ z₁ tl (tl' z₁)) st)
                                     (StStreamFunctions.flatmapInnerSt (λ z₁ x → StStreamFunctions.flatmapSt f₂ (f₁ z₁ x)) z' tl (StStreamFunctions.flatmapInnerSt f₂ z' tl' st))
      StStream≅.stream (flatmapFlatmap'' z' tl tl' st) with StStream.stream st
      StStream≅.stream (flatmapFlatmap'' z' tl tl' st) | nil = eqskip (flatmapFlatmap' z' tl (tl' z'))
      StStream≅.stream (flatmapFlatmap'' z' tl tl' st) | cons (just c , z'') tl'' = eqcons refl (flatmapFlatmap'' z'' tl tl' (tl'' z''))
      StStream≅.stream (flatmapFlatmap'' z' tl tl' st) | cons (nothing , z'') tl'' = eqskip (flatmapFlatmap'' z'' tl tl' (tl'' z''))

  module _ {A B Z₁ Z₂ : Set} where
    private
      step : ∀ (upb₁ : ℕ) → (step₁ : Z₁ → Fin upb₁ → A × Z₁) →  (upb₂ : ℕ) → (step₂ : Z₂ → Fin upb₂ → B × Z₂) → (Z₁ × Z₂) → Fin upb₁ → Fin upb₂ → ((A × B) × (Z₁ × Z₂))
      step upb₁ step₁ upb₂ step₂ (z₁ , z₂) i₁ i₂ with step₁ z₁ i₁
      step upb₁ step₁ upb₂ step₂ (z₁ , z₂) i₁ i₂ | a , z₁' with step₂ z₂ i₂
      step upb₁ step₁ upb₂ step₂ (z₁ , z₂) i₁ i₂ | a , z₁' | b , z₂' = ((a , b) , (z₁' , z₂'))

      pullArrayLte : ∀ {A Z : Set} → (n m : ℕ) → (lte : n ≤ m) → Z → (Z → Fin n → Fin m → A × Z) → StStream A Z
      StStream.stream (pullArrayLte .0 m z≤n z step) = nil
      StStream.stream (pullArrayLte .(suc _) .(suc _) (s≤s {n} {m} lte) z step) with step z zero zero
      StStream.stream (pullArrayLte .(suc _) .(suc _) (s≤s {n} {m} lte) z step) | x , z' = cons (just x , z') (λ z' → pullArrayLte n m lte z' (λ z n m → step z (suc n) (suc m))) 

    zipPullArrayPullArray : ∀ (z₁ : Z₁) (upb₁ : ℕ) (step₁ : Z₁ → Fin upb₁ → A × Z₁) (z₂ : Z₂) (upb₂ : ℕ) (step₂ : Z₂ → Fin upb₂ → B × Z₂) → (lte : upb₁ ≤ upb₂) →
                              StStream≅ (zipSt (pullArray z₁ upb₁ step₁) (pullArray z₂ upb₂ step₂)) (pullArrayLte upb₁ upb₂ lte (z₁ , z₂) (step upb₁ step₁ upb₂ step₂))
    StStream≅.stream (zipPullArrayPullArray z₁ upb₁ step₁ z₂ upb₂ step₂ lte) with lte
    StStream≅.stream (zipPullArrayPullArray z₁ .zero step₁ z₂ upb₂ step₂ lte) | z≤n = eqnil
    StStream≅.stream (zipPullArrayPullArray z₁ .(suc _) step₁ z₂ .(suc _) step₂ lte) | s≤s lte' with step₁ z₁ zero
    StStream≅.stream (zipPullArrayPullArray z₁ .(suc _) step₁ z₂ .(suc _) step₂ lte) | s≤s lte' | a , z₁' with step₂ z₂ zero
    StStream≅.stream (zipPullArrayPullArray z₁ .(suc _) step₁ z₂ .(suc _) step₂ lte) | s≤s {m} {n} lte' | a , z₁' | b , z₂' = eqcons refl (zipPullArrayPullArray z₁' m (λ z m → step₁ z (suc m)) z₂' n (λ z n → step₂ z (suc n)) lte')

  module _ {A B Z₁ Z₂ : Set} (step₁ : Z₁ → Maybe A × Z₁) (step₂ : Z₂ → Maybe B × Z₂) (side : {z₂ z₂' : Z₂} → step₂ z₂ ≡ (nothing , z₂') → Σ Z₂ (λ z₂'' → step₂ z₂' ≡ (nothing , z₂''))) where
    private
       step : (Z₁ × Z₂) → Maybe (A × B) × (Z₁ × Z₂)
       step (z₁ , z₂) with step₁ z₁
       step (z₁ , z₂) | just a , z₁' with step₂ z₂
       step (z₁ , z₂) | just a , z₁' | just b , z₂' = (just (a , b)) , (z₁' , z₂')
       step (z₁ , z₂) | just a , z₁' | nothing , z₂' = nothing , (z₁' , z₂')
       step (z₁ , z₂) | nothing , z₁' =  nothing , (z₁' , z₂)

       lemma : ∀ {z₁ : Z₁} {z₂ z₂' : Z₂} (prf : step₂ z₂ ≡ (nothing , z₂')) → StStream≈ (mkStream nil) (unroll step (z₁ , z₂'))
       StStream≈.stream (lemma {z₁ = z₁} prf) with step₁ z₁
       StStream≈.stream (lemma prf) | just a , z₁' with side prf
       StStream≈.stream (lemma prf) | just a , z₁' | z₂'' , prf' rewrite prf' = eqnilskip (lemma prf')
       StStream≈.stream (lemma prf) | nothing , z₁' = eqnilskip (lemma prf)

       lemma' : ∀ {z₁ : Z₁} {z₂ z₂' : Z₂} (prf : step₂ z₂ ≡ (nothing , z₂')) → StStream≈ (mkStream nil) (unroll step (z₁ , z₂))
       StStream≈.stream (lemma' {z₁ = z₁} prf) with step₁ z₁
       StStream≈.stream (lemma' prf) | just a , z₁' with side prf
       StStream≈.stream (lemma' prf) | just a , z₁' | z₂'' , prf' rewrite prf = eqnilskip (lemma' prf')
       StStream≈.stream (lemma' prf) | nothing , z₁' = eqnilskip (lemma' prf)
       
       lemma'' : ∀ {z₁ : Z₁} {z₂ z₂' : Z₂} (prf : step₂ z₂ ≡ (nothing , z₂')) → StStream≈ (zipSt (unroll step₁ z₁) (unroll step₂ z₂')) (mkStream nil)
       StStream≈.stream (lemma'' {z₁ = z₁} prf) with step₁ z₁
       StStream≈.stream (lemma'' prf) | just a , z₁' with side prf
       StStream≈.stream (lemma'' prf) | just a , z₁' | z₂'' , prf' rewrite prf' = eqskipnil (lemma'' prf')
       StStream≈.stream (lemma'' prf) | nothing , z₁' with side prf
       StStream≈.stream (lemma'' prf) | nothing , z₁' | z₂'' , prf' rewrite prf' = eqskipnil (lemma'' prf')

    zipUnrollUnroll : ∀ (z₁ : Z₁) (z₂ : Z₂) → StStream≈ (zipSt (unroll step₁ z₁) (unroll step₂ z₂)) (unroll step (z₁ , z₂))
    StStream≈.stream (zipUnrollUnroll z₁ z₂) with step₁ z₁
    StStream≈.stream (zipUnrollUnroll z₁ z₂) | just a , z₁' with step₂ z₂   | inspect step₂ z₂
    StStream≈.stream (zipUnrollUnroll z₁ z₂) | just a , z₁' | just b , z₂'  | [ prf ] = eqcons refl (zipUnrollUnroll z₁' z₂')
    StStream≈.stream (zipUnrollUnroll z₁ z₂) | just a , z₁' | nothing , z₂' | [ prf ] = eqskip (trans≈ (lemma'' prf) (lemma prf))
    StStream≈.stream (zipUnrollUnroll z₁ z₂) | nothing , z₁' with step₂ z₂ | inspect step₂ z₂
    StStream≈.stream (zipUnrollUnroll z₁ z₂) | nothing , z₁' | just b , z₂' | [ prf ] = eqskip (zipUnrollUnroll z₁' z₂)
    StStream≈.stream (zipUnrollUnroll z₁ z₂) | nothing , z₁' | nothing , z₂' | [ prf ] = eqskip (trans≈ (lemma'' prf) (lemma' prf))

  module _ {A B Z₁ Z₂ : Set} (step₁ : Z₁ → Maybe A × Z₁) (step₂ : Z₂ → Maybe B × Z₂) where
    private
      step :  (Maybe (A ⊎ B) × Z₁) × Z₂ → Maybe (A × B) × (Maybe (A ⊎ B) × Z₁) × Z₂
      step ((just (inj₁ a) , z₁) , z₂) with step₂ z₂
      step ((just (inj₁ a) , z₁) , z₂) | just b , z₂' = (just (a , b)) , ((nothing , z₁) , z₂')
      step ((just (inj₁ a) , z₁) , z₂) | nothing , z₂' = nothing , ((just (inj₁ a) , z₁) , z₂')
      step ((just (inj₂ b) , z₁) , z₂) with step₁ z₁
      step ((just (inj₂ b) , z₁) , z₂) | just a , z₁' = (just (a , b)) , ((nothing , z₁') , z₂)
      step ((just (inj₂ b) , z₁) , z₂) | nothing , z₁' = nothing , ((just (inj₂ b) , z₁') , z₂)
      step ((nothing , z₁) , z₂) with step₁ z₁
      step ((nothing , z₁) , z₂) | just a , z₁' with step₂ z₂
      step ((nothing , z₁) , z₂) | just a , z₁' | just b , z₂' = (just (a , b)) , ((nothing , z₁') , z₂')
      step ((nothing , z₁) , z₂) | just a , z₁' | nothing , z₂' = nothing , (((just (inj₁ a)) , z₁') , z₂')
      step ((nothing , z₁) , z₂) | nothing , z₁' with step₂ z₂
      step ((nothing , z₁) , z₂) | nothing , z₁' | just b , z₂' = nothing , ((just (inj₂ b) , z₁') , z₂')
      step ((nothing , z₁) , z₂) | nothing , z₁' | nothing , z₂' = nothing , ((nothing , z₁') , z₂')

      justa≡justa'→a≡a' : {A : Set} {a a' : A} → just a  ≡ just a' → a ≡ a'
      justa≡justa'→a≡a' refl = refl

    mutual
      lemma : ∀ (z₁ z₁' : Z₁) (z₂' : Z₂) (a : A) → (prf :  Reveal step₁ · z₁ is (just a , z₁')) →
                StStream≅ (zipSt (unroll step₁ z₁) (unroll step₂ z₂'))
                          (abstractSt (adjust ((λ { ((z , z₁) , z₂) → z , z₁ , z₂ }) , (λ { (z , z₁ , z₂) → (z , z₁) , z₂ })) (unroll step ((just (inj₁ a) , z₁') , z₂'))))
      StStream≅.stream (lemma z₁ z₁' z₂' a prf) with step₁ z₁ | inspect step₁ z₁
      StStream≅.stream (lemma z₁ z₁' z₂' a [ prf ]) | just a' , z₁'' | [ prf' ] with step₂ z₂'  
      StStream≅.stream (lemma z₁ z₁' z₂' a [ prf ]) | just a' , z₁'' | [ prf' ] | just b , z₂'' with trans (sym prf) prf'
      StStream≅.stream (lemma z₁ z₁' z₂' a [ prf ]) | just a' , z₁'' | [ prf' ] | just b , z₂'' | justam,z₁≡justa',z₁'' rewrite (justa≡justa'→a≡a' (cong proj₁ justam,z₁≡justa',z₁'')) rewrite (cong proj₂ justam,z₁≡justa',z₁'')  = eqcons refl (zipUnrollUnrollExt z₁'' z₂'')
      StStream≅.stream (lemma z₁ z₁' z₂' a [ prf ]) | just a' , z₁'' | [ prf' ] | nothing , z₂'' = eqskip (lemma z₁ z₁' z₂'' a [ prf ])
      StStream≅.stream (lemma z₁ z₁' z₂' a [ prf ]) | nothing , z₁'' | [ prf' ] with trans (cong proj₁ (sym prf)) (cong proj₁ prf')
      StStream≅.stream (lemma z₁ z₁' z₂' a [ prf ]) | nothing , z₁'' | [ prf' ] | ()

      lemma' : ∀ (z₁' : Z₁) (z₂ z₂' : Z₂) (b : B) (prf :  Reveal step₂ · z₂ is (just b , z₂'))  →
               StStream≅ (zipSt (unroll step₁ z₁') (unroll step₂ z₂))
                         (abstractSt (adjust ((λ { ((z , z₁) , z₂) → z , z₁ , z₂ }) , (λ { (z , z₁ , z₂) → (z , z₁) , z₂ })) (unroll step ((just (inj₂ b) , z₁') , z₂'))))         
      StStream≅.stream (lemma' z₁' z₂ z₂' b prf) with step₁ z₁'
      StStream≅.stream (lemma' z₁' z₂ z₂' b prf) | just a , z₁'' with step₂ z₂ | inspect step₂ z₂
      StStream≅.stream (lemma' z₁' z₂ z₂' b [ prf ]) | just a , z₁'' | just b' , z₂'' | [ prf' ] with trans (sym prf) prf'
      StStream≅.stream (lemma' z₁' z₂ z₂' b [ prf ]) | just a , z₁'' | just b' , z₂'' | [ prf' ] | justb,z₂'≡justb',z₂'' rewrite (justa≡justa'→a≡a' (cong proj₁ justb,z₂'≡justb',z₂'')) rewrite (cong proj₂ (justb,z₂'≡justb',z₂'')) = eqcons refl (zipUnrollUnrollExt z₁'' z₂'')
      StStream≅.stream (lemma' z₁' z₂ z₂' b [ prf ]) | just a , z₁'' | nothing , z₂'' | [ prf' ] with trans (cong proj₁ (sym prf)) (cong proj₁ prf')
      StStream≅.stream (lemma' z₁' z₂ z₂' b [ prf ]) | just a , z₁'' | nothing , z₂'' | [ prf' ] | ()
      StStream≅.stream (lemma' z₁' z₂ z₂' b prf) | nothing , z₁'' with step₂ z₂ | inspect step₂ z₂
      StStream≅.stream (lemma' z₁' z₂ z₂' b [ prf ]) | nothing , z₁'' | just b' , z₂'' | [ prf' ] = eqskip (lemma' z₁'' z₂ z₂' b [ prf ])
      StStream≅.stream (lemma' z₁' z₂ z₂' b [ prf ]) | nothing , z₁'' | nothing , z₂'' | [ prf' ] with trans (cong proj₁ (sym prf)) (cong proj₁ prf')
      StStream≅.stream (lemma' z₁' z₂ z₂' b [ prf ]) | nothing , z₁'' | nothing , z₂'' | [ prf' ] | ()
  
      zipUnrollUnrollExt : ∀ (z₁ : Z₁) (z₂ : Z₂) → StStream≅ (zipSt (unroll step₁ z₁) (unroll step₂ z₂)) (unroll step ((nothing , z₁) , z₂) |>
                                                                                                               adjust ((λ { ((z , z₁) , z₂) → (z , (z₁ , z₂)) }) ,
                                                                                                                       (λ { (z , (z₁ , z₂)) → ((z , z₁) , z₂) })) |> abstractSt )
      StStream≅.stream (zipUnrollUnrollExt z₁ z₂) with step₁ z₁   | inspect step₁ z₁
      StStream≅.stream (zipUnrollUnrollExt z₁ z₂) | just a , z₁'  | [ prf ] with step₂ z₂
      StStream≅.stream (zipUnrollUnrollExt z₁ z₂) | just a , z₁'  | [ prf ] | just b , z₂' = eqcons refl (zipUnrollUnrollExt z₁' z₂')
      StStream≅.stream (zipUnrollUnrollExt z₁ z₂) | just a , z₁'  | [ prf ] | nothing , z₂' = eqskip (lemma z₁ z₁' z₂' a [ prf ])
      StStream≅.stream (zipUnrollUnrollExt z₁ z₂) | nothing , z₁' | [ prf ] with step₂ z₂   | inspect step₂ z₂
      StStream≅.stream (zipUnrollUnrollExt z₁ z₂) | nothing , z₁' | [ prf ] | just b , z₂'  | [ prf' ] = eqskip (lemma' z₁' z₂ z₂' b [ prf' ])
      StStream≅.stream (zipUnrollUnrollExt z₁ z₂) | nothing , z₁' | [ prf ] | nothing , z₂' | [ prf' ] = eqskip (zipUnrollUnrollExt z₁' z₂')
  
  module _ {A Z : Set} (step : Z → Maybe A × Z) (pred : Z → Bool) (side : (z : Z) → step z |> λ { (just x , z') → ⊤ ; (nothing , z') → pred z' |> λ { false → ⊤ ; true → ⊥ } })  where
    private
      step' : Z → Maybe A × Z
      step' z with side z
      step' z | c with step z
      step' z | tt | just a , z' = just a , z'
      step' z | c | nothing , z' with pred z'
      step' z | tt | nothing , z' | false = nothing , z'
      
    unrollLinear : ∀ (z : Z) → StStream≅ (unroll step z |> guard pred) (unroll step' z |> guard pred)
    StStream≅.stream (unrollLinear z) with side z
    StStream≅.stream (unrollLinear z) | c with step z
    StStream≅.stream (unrollLinear z) | c | just a , z' with pred z'
    StStream≅.stream (unrollLinear z) | c | just a , z' | false = eqnil
    StStream≅.stream (unrollLinear z) | c | just a , z' | true = eqcons refl (unrollLinear z')
    StStream≅.stream (unrollLinear z) | c | nothing , z' with pred z' | inspect pred z'
    StStream≅.stream (unrollLinear z) | c | nothing , z' | false | [ prf ] rewrite prf = eqnil

  module _ {A B Z₁ Z₂ : Set} (step₁ : Z₁ → Maybe A × Z₁) (side : (z₁ : Z₁) → step₁ z₁ |> λ { (just a , z₁') → ⊤ ; (nothing , z₁') → ⊥ }) where
    private
      f : Z₁ × Z₂ → B → Maybe (A × B) × (Z₁ × Z₂)
      f (z₁ , z₂) b with side z₁
      f (z₁ , z₂) b | c with step₁ z₁
      f (z₁ , z₂) b | tt | just a , z₁' = just (a , b) , (z₁' , z₂)

      justa≡justa'→a≡a' : {a a' : A} → just a ≡ just a' → a ≡ a'
      justa≡justa'→a≡a' refl = refl

    mutual
      zipLinear₁Lemma : ∀ (z₁ z₁' : Z₁) (z₂ : Z₂) (tl₂ : Z₂ → StStream B Z₂) (a : A) (b : B) → (prf : Reveal step₁ · z₁ is (just a , z₁')) →
                              StStreamEq (A × B) (Z₁ × Z₂) (cons (just (a , b) , z₁' , z₂) (λ { (z₁ , z₂) → zipSt (unroll step₁ z₁) (tl₂ z₂) }))
                                                           (cons (f (z₁ , z₂) b) (λ { (z₁ , z₂) → mapFilterSt f (initSt z₁ (tl₂ z₂)) }))
      zipLinear₁Lemma z₁ z₁' z₂ tl₂ a b prf with side z₁
      zipLinear₁Lemma z₁ z₁' z₂ tl₂ a b prf | c with step₁ z₁ | inspect step₁ z₁ 
      zipLinear₁Lemma z₁ z₁' z₂ tl₂ a b [ prf ] | c | just a' , z₁'' | [ prf' ] with trans (sym prf) prf'
      zipLinear₁Lemma z₁ z₁' z₂ tl₂ a b [ prf ] | c | just a' , z₁'' | [ prf' ] | justa,z₁'≡justa'z₁'' rewrite (justa≡justa'→a≡a' (cong proj₁ justa,z₁'≡justa'z₁'')) rewrite (cong proj₂ justa,z₁'≡justa'z₁'') = eqcons refl (zipLinear₁ z₁'' (tl₂ z₂))
      
      zipLinear₁ : ∀ (z₁ : Z₁) (st₂ : StStream B Z₂) → StStream≅ (zipSt (unroll step₁ z₁) st₂) (initSt z₁ st₂ |> mapFilterSt f)
      StStream≅.stream (zipLinear₁ z₁ st₂) with side z₁
      StStream≅.stream (zipLinear₁ z₁ st₂) | c with step₁ z₁  | inspect step₁ z₁
      StStream≅.stream (zipLinear₁ z₁ st₂) | c | just a , z₁' | prf with StStream.stream st₂
      StStream≅.stream (zipLinear₁ z₁ st₂) | c | just a , z₁' | prf | nil = eqnil
      StStream≅.stream (zipLinear₁ z₁ st₂) | c | just a , z₁' | prf | cons (just b , z₂) tl₂  = zipLinear₁Lemma z₁ z₁' z₂ tl₂ a b prf
      StStream≅.stream (zipLinear₁ z₁ st₂) | c | just a , z₁' | prf | cons (nothing , z₂) tl₂ = eqskip (zipLinear₁ z₁ (tl₂ z₂))

  module _ {A B Z₁ Z₂ : Set} (step₁ : Z₁ → Maybe A × Z₁) (side : {z₁ z₁' : Z₁} → step₁ z₁ ≡ (nothing , z₁') → Σ Z₁ (λ z₁'' → step₁ z₁' ≡ (nothing , z₁''))) where
    private
      f : Z₁ × Z₂ → B → Maybe (A × B) × (Z₁ × Z₂)
      f (z₁ , z₂) b with step₁ z₁
      f (z₁ , z₂) b | just a , z₁' = (just (a , b)) , (z₁' , z₂)
      f (z₁ , z₂) b | nothing , z₁' = nothing , (z₁' , z₂)

    mutual
      leftnilLemma : ∀ {z₁ : Z₁} (st : StStream B Z₂) (prf : Σ Z₁ (λ z₁' → step₁ z₁ ≡ (nothing , z₁'))) → StStream≈ (mkStream nil) (mapFilterSt f (initSt z₁ st))
      StStream≈.stream (leftnilLemma st prf) with StStream.stream st
      StStream≈.stream (leftnilLemma st prf) | nil = eqnil
      StStream≈.stream (leftnilLemma st (_ , prf)) | cons (just b , z₂) tl₂ rewrite prf = eqnilskip (leftnilLemma (tl₂ z₂) (side prf))
      StStream≈.stream (leftnilLemma st prf) | cons (nothing , z₂) tl₂ = eqnilskip (leftnilLemma (tl₂ z₂) prf)

      rightnilLemma : ∀ {z₁ : Z₁} (st : StStream B Z₂) (prf : Σ Z₁ (λ z₁' → step₁ z₁ ≡ (nothing , z₁'))) → StStream≈ (zipSt (unroll step₁ z₁) st) (mkStream nil)
      StStream≈.stream (rightnilLemma st (_ , prf)) with StStream.stream st | inspect StStream.stream st
      StStream≈.stream (rightnilLemma st (_ , prf)) | nil | [ prf' ] rewrite prf rewrite prf' = eqnil
      StStream≈.stream (rightnilLemma st (_ , prf)) | cons (just x , z₂) tl₂ | [ prf' ] rewrite prf rewrite prf' = eqskipnil (rightnilLemma st (side prf))
      StStream≈.stream (rightnilLemma st (_ , prf)) | cons (nothing , z₂) tl₂ | [ prf' ] rewrite prf rewrite prf' = eqskipnil (rightnilLemma (tl₂ z₂) (side prf))
    
      zipLinear₂Lemma' : {z₁ z₁' : Z₁} (st₂ st₂' : StStream B Z₂) (prf : Σ Z₁ (λ  z₁' → step₁ z₁ ≡ (nothing , z₁'))) (prf' : Σ Z₁ (λ  z₁'' → step₁ z₁' ≡ (nothing , z₁''))) → StStream≈ (zipSt (unroll step₁ z₁') st₂) (mapFilterSt f (initSt z₁ st₂'))
      StStream≈.stream (zipLinear₂Lemma' st₂ st₂' prf prf') with StStream.stream st₂'
      StStream≈.stream (zipLinear₂Lemma' st₂ st₂' prf (z₁'' , prf')) | nil rewrite prf' with StStream.stream st₂
      StStream≈.stream (zipLinear₂Lemma' st₂ st₂' prf (z₁'' , prf')) | nil | nil = eqnil
      StStream≈.stream (zipLinear₂Lemma' st₂ st₂' prf (z₁'' , prf')) | nil | cons (just b , z₂) tl₂ = eqskipnil (rightnilLemma st₂ (side prf'))
      StStream≈.stream (zipLinear₂Lemma' st₂ st₂' prf (z₁'' , prf')) | nil | cons (nothing , z₂) tl₂ = eqskipnil (rightnilLemma (tl₂ z₂) (side prf'))
      StStream≈.stream (zipLinear₂Lemma' st₂ st₂' (z₁' , prf) (z₁'' , prf')) | cons (just b , z₂') tl₂' rewrite prf' rewrite prf with StStream.stream st₂
      StStream≈.stream (zipLinear₂Lemma' st₂ st₂' (_ , prf) (z₁'' , prf')) | cons (just b , z₂') tl₂' | nil = eqnilskip  (leftnilLemma (tl₂' z₂') (side prf))
      StStream≈.stream (zipLinear₂Lemma' st₂ st₂' (z₁' , prf) (z₁'' , prf')) | cons (just b , z₂') tl₂' | cons (just b' , z₂) tl₂ = eqskip (zipLinear₂Lemma' st₂ (tl₂' z₂') (side prf) (side prf'))
      StStream≈.stream (zipLinear₂Lemma' st₂ st₂' (z₁' , prf) (z₁'' , prf')) | cons (just b , z₂') tl₂' | cons (nothing , z₂) tl₂ = eqskip (zipLinear₂Lemma' (tl₂ z₂) (tl₂' z₂') (side prf) (side prf'))
      StStream≈.stream (zipLinear₂Lemma' st₂ st₂' prf (z₁'' , prf')) | cons (nothing , z₂') tl₂' rewrite prf' with StStream.stream st₂
      StStream≈.stream (zipLinear₂Lemma' st₂ st₂' prf (z₁'' , prf')) | cons (nothing , z₂') tl₂' | nil = eqnilskip (leftnilLemma (tl₂' z₂') prf)
      StStream≈.stream (zipLinear₂Lemma' st₂ st₂' prf (z₁'' , prf')) | cons (nothing , z₂') tl₂' | cons (just b , z₂) tl₂ = eqskip (zipLinear₂Lemma' st₂ (tl₂' z₂') prf (side prf'))
      StStream≈.stream (zipLinear₂Lemma' st₂ st₂' prf (z₁'' , prf')) | cons (nothing , z₂') tl₂' | cons (nothing , z₂) tl₂ = eqskip (zipLinear₂Lemma' (tl₂ z₂) (tl₂' z₂') prf (side prf'))

      zipLinear₂Lemma : {z₁ z₁' : Z₁} (st₂ st₂' : StStream B Z₂) (prf : step₁ z₁ ≡ (nothing , z₁')) → StStream≈ (zipSt (unroll step₁ z₁') st₂) (mapFilterSt f (initSt z₁' st₂'))
      zipLinear₂Lemma st₂ st₂' prf = zipLinear₂Lemma' st₂ st₂' (side prf) (side prf)

      zipLinear₂ : ∀ (z₁ : Z₁) (st₂ : StStream B Z₂) → StStream≈ (zipSt (unroll step₁ z₁) st₂) (initSt z₁ st₂ |> mapFilterSt f)
      StStream≈.stream (zipLinear₂ z₁ st₂) with step₁ z₁ | inspect step₁ z₁
      StStream≈.stream (zipLinear₂ z₁ st₂) | just a , z₁' | prf with StStream.stream st₂
      StStream≈.stream (zipLinear₂ z₁ st₂) | just a , z₁' | prf | nil = eqnil
      StStream≈.stream (zipLinear₂ z₁ st₂) | just a , z₁' | [ prf ] | cons (just b , z₂) tl₂ rewrite prf = eqcons refl (zipLinear₂ z₁' (tl₂ z₂))
      StStream≈.stream (zipLinear₂ z₁ st₂) | just a , z₁' | prf | cons (nothing , z₂) tl₂ = eqskip (zipLinear₂ z₁ (tl₂ z₂))
      StStream≈.stream (zipLinear₂ z₁ st₂) | nothing , z₁' | prf with StStream.stream st₂
      StStream≈.stream (zipLinear₂ z₁ st₂) | nothing , z₁' | prf | nil = eqnil
      StStream≈.stream (zipLinear₂ z₁ st₂) | nothing , z₁' | [ prf ] | cons (just b , z₂) tl₂ rewrite prf = eqskip (zipLinear₂Lemma st₂ (tl₂ z₂) prf)
      StStream≈.stream (zipLinear₂ z₁ st₂) | nothing , z₁' | [ prf ] | cons (nothing , z₂) tl₂ with side prf
      StStream≈.stream (zipLinear₂ z₁ st₂) | nothing , z₁' | [ prf ] | cons (nothing , z₂) tl₂ | z₁'' , prf' = eqskip (zipLinear₂Lemma' (tl₂ z₂) (tl₂ z₂) (z₁' , prf) (z₁'' , prf')) 

  module _ {A B Z₁ Z₂ : Set} (pred : Z₁ → Bool) where

    zipGuard : ∀ (st₁ : StStream A Z₁) (st₂ : StStream B Z₂) → StStream≅ (zipSt (guard pred st₁) st₂) (zipSt st₁ st₂ |> guard (λ { (z₁ , z₂) → pred z₁ }))
    StStream≅.stream (zipGuard st₁ st₂) with StStream.stream st₁
    StStream≅.stream (zipGuard st₁ st₂) | nil = eqnil
    StStream≅.stream (zipGuard st₁ st₂) | cons (just a , z₁) tl₁ with pred z₁ | inspect pred z₁
    StStream≅.stream (zipGuard st₁ st₂) | cons (just a , z₁) tl₁ | false | [ prf ] with StStream.stream st₂
    StStream≅.stream (zipGuard st₁ st₂) | cons (just a , z₁) tl₁ | false | [ prf ] | nil = eqnil
    StStream≅.stream (zipGuard st₁ st₂) | cons (just a , z₁) tl₁ | false | [ prf ] | cons (just b , z₂) tl₂ rewrite prf = eqnil
    StStream≅.stream (zipGuard st₁ st₂) | cons (just a , z₁) tl₁ | false | [ prf ] | cons (nothing , z₂) tl₂ rewrite prf = eqnil
    StStream≅.stream (zipGuard st₁ st₂) | cons (just a , z₁) tl₁ | true | [ prf ] with StStream.stream st₂
    StStream≅.stream (zipGuard st₁ st₂) | cons (just a , z₁) tl₁ | true | [ prf ] | nil = eqnil
    StStream≅.stream (zipGuard st₁ st₂) | cons (just a , z₁) tl₁ | true | [ prf ] | cons (just b , z₂) tl₂ rewrite prf = eqcons refl (zipGuard (tl₁ z₁) (tl₂ z₂))
    StStream≅.stream (zipGuard st₁ st₂) | cons (just a , z₁) tl₁ | true | [ prf ] | cons (nothing , z₂) tl₂ rewrite prf = eqskip (zipGuard st₁ (tl₂ z₂))
    StStream≅.stream (zipGuard st₁ st₂) | cons (nothing , z₁) tl₁ with pred z₁ | inspect pred z₁
    StStream≅.stream (zipGuard st₁ st₂) | cons (nothing , z₁) tl₁ | false | [ prf ] with StStream.stream st₂
    StStream≅.stream (zipGuard st₁ st₂) | cons (nothing , z₁) tl₁ | false | [ prf ] | nil = eqnil
    StStream≅.stream (zipGuard st₁ st₂) | cons (nothing , z₁) tl₁ | false | [ prf ] | cons (just b , z₂) tl₂ rewrite prf = eqnil
    StStream≅.stream (zipGuard st₁ st₂) | cons (nothing , z₁) tl₁ | false | [ prf ] | cons (nothing , z₂) tl₂ rewrite prf = eqnil
    StStream≅.stream (zipGuard st₁ st₂) | cons (nothing , z₁) tl₁ | true  | [ prf ] with StStream.stream st₂
    StStream≅.stream (zipGuard st₁ st₂) | cons (nothing , z₁) tl₁ | true | [ prf ] | nil = eqnil
    StStream≅.stream (zipGuard st₁ st₂) | cons (nothing , z₁) tl₁ | true | [ prf ] | cons (just b , z₂) tl₂ rewrite prf = eqskip (zipGuard (tl₁ z₁) st₂)
    StStream≅.stream (zipGuard st₁ st₂) | cons (nothing , z₁) tl₁ | true | [ prf ] | cons (nothing , z₂) tl₂ rewrite prf = eqskip (zipGuard (tl₁ z₁) (tl₂ z₂))

  module _ {A B Z Zp : Set} (step : Z → Maybe A × Z) (pred : Z → Bool) (stepi : Z → A → (Zp × Z) → Maybe B × (Zp × Z)) (zi : Z → A → Zp × Z) (predi : Z → A → (Zp × Z) → Bool)
                            (side₁ : (z : Z) → step z |> λ { (just x , z') → ⊤ ; (nothing , z') → pred z' |> λ { false → ⊤ ; true → ⊥ } })
                            (side₂ : (x : A) (z₀ : Z) (z : Zp × Z) → stepi z₀ x z |> λ { (just x , z') → ⊤ ; (nothing , z') → predi z₀ x z' |> λ { false → ⊤ ; true → ⊥ } }) where
    private
      Stepix : Z → A → Set
      Stepix z a = Σ ((Zp × Z) → Maybe B × (Zp × Z)) (λ stepix → stepix ≡ (λ zn → stepi z a zn))
      
      Predix : Z → A → Set
      Predix z a = Σ ((Zp × Z) → Bool) (λ predix → predix ≡ (λ zn → predi z a zn))

      Stepi×Predi : Set
      Stepi×Predi = Σ (Z × A) λ { (z , a) → Stepix z a × Predix z a }
      
      step' : Maybe (Stepi×Predi × Zp × Z) × Z → Maybe B × Maybe (Stepi×Predi ×  Zp × Z) × Z
      step' (just (((zc , a) , (stepix , stepeq) , (predix , predeq)) , (zp , z)) , z₀) with side₂ a zc (zp , z)
      step' (just (((zc , a) , (stepix , stepeq) , (predix , predeq)) , (zp , z)) , z₀) | c rewrite (sym stepeq) with stepix (zp , z) 
      step' (just (((zc , a) , (stepix , stepeq) , (predix , predeq)) , (zp , z)) , z₀) | c  | just y , zp' , z' rewrite (sym predeq) with predix (zp' , z') 
      step' (just (((zc , a) , (stepix , stepeq) , (predix , predeq)) , (zp , z)) , z₀) | tt | just y , zp' , z' | false = (nothing , (nothing  , z₀))
      step' (just (((zc , a) , (stepix , stepeq) , (predix , predeq)) , (zp , z)) , z₀) | tt | just y , zp' , z' | true = just y , (just (((zc , a) , (stepix , stepeq) , (predix , predeq)) , (zp' , z'))) , z' 
      step' (just (((zc , a) , (stepix , stepeq) , (predix , predeq)) , (zp , z)) , z₀) | c  | nothing , zp' , z' rewrite (sym predeq) with predix (zp' , z') 
      step' (just (((zc , a) , (stepix , stepeq) , (predix , predeq)) , (zp , z)) , z₀) | tt | nothing , zp' , z' | false =  nothing , (nothing  , z₀)
      step' (just (((zc , a) , (stepix , stepeq) , predix , predeq) , zp , z) , z₀) | () | nothing , zp' , z' | true
      step' (nothing , z) with side₁ z
      step' (nothing , z) | c with step z 
      step' (nothing , z) | c | just x , z' with pred z'
      step' (nothing , z) | c | just x , z' | false = (nothing , (nothing , z'))
      step' (nothing , z) | c | just x , z' | true  = let stepix = (λ zn → stepi z' x zn) in
                                                      let predix = (λ zn → predi z' x zn) in
                                                      let (zp , z'') = zi z' x in
                                                      nothing , ((just (((z' , x) , ((stepix , refl) , predix , refl)) , zp , z'')) , z')
      step' (nothing , z) | c | nothing , z' with pred z' 
      step' (nothing , z) | c | nothing , z' | false = (nothing , (nothing , z')) 
      step' (nothing , z) | () | nothing , z' | true


    mutual
      flatmapLinear₁Lemma : ∀ (z₀ : Z) (x : A) (z : Z) ((zp' , z') : Zp × Z) →
                   StStream≅ (guard pred (StStreamFunctions.flatmapInnerSt (λ z₁ x₁ → abstractSt (guard (predi z₁ x₁) (unroll (stepi z₁ x₁) (zi z₁ x₁)))) z
                                                               (λ z₁ → guard pred (unroll step z₁))
                                                               (abstractSt (guard (predi z₀ x) (unroll (stepi z₀ x) (zp' , z'))))))
                             (abstractSt (guard (λ { (_ , z) → pred z }) (unroll step' (just (((z₀ , x) , (stepi z₀ x , refl) , (predi z₀ x , refl)) , (zp' , z')) , z))))
      StStream≅.stream (flatmapLinear₁Lemma z₀ x z (zp' , z')) with side₂ x z₀ (zp' , z')
      StStream≅.stream (flatmapLinear₁Lemma z₀ x z (zp' , z')) | c with stepi z₀ x (zp' , z')
      StStream≅.stream (flatmapLinear₁Lemma z₀ x z (zp' , z')) | c | just b , zp'' , z'' with predi z₀ x (zp'' , z'')
      StStream≅.stream (flatmapLinear₁Lemma z₀ x z (zp' , z')) | c | just b , zp'' , z'' | false with pred z
      StStream≅.stream (flatmapLinear₁Lemma z₀ x z (zp' , z')) | c | just b , zp'' , z'' | false | false = eqnil
      StStream≅.stream (flatmapLinear₁Lemma z₀ x z (zp' , z')) | c | just b , zp'' , z'' | false | true = eqskip (flatmapLinear₁ z)
      StStream≅.stream (flatmapLinear₁Lemma z₀ x z (zp' , z')) | c | just b , zp'' , z'' | true with pred z''
      StStream≅.stream (flatmapLinear₁Lemma z₀ x z (zp' , z')) | c | just b , zp'' , z'' | true | false = eqnil
      StStream≅.stream (flatmapLinear₁Lemma z₀ x z (zp' , z')) | c | just b , zp'' , z'' | true | true = eqcons refl (flatmapLinear₁Lemma z₀ x z'' (zp'' , z''))
      StStream≅.stream (flatmapLinear₁Lemma z₀ x z (zp' , z')) | c | nothing , zp'' , z'' with predi z₀ x (zp'' , z'')
      StStream≅.stream (flatmapLinear₁Lemma z₀ x z (zp' , z')) | c | nothing , zp'' , z'' | false with pred z
      StStream≅.stream (flatmapLinear₁Lemma z₀ x z (zp' , z')) | c | nothing , zp'' , z'' | false | false = eqnil
      StStream≅.stream (flatmapLinear₁Lemma z₀ x z (zp' , z')) | c | nothing , zp'' , z'' | false | true = eqskip (flatmapLinear₁ z)
     
      flatmapLinear₁ : ∀ (z : Z) → StStream≅ (unroll step z |> guard pred |> flatmapSt (λ z x → unroll (stepi z x) (zi z x) |> guard (predi z x) |> abstractSt) |> guard (λ { z → pred z }))
                                             (unroll step' (nothing , z) |> guard (λ { (_ , z) → pred z }) |> abstractSt)
      StStream≅.stream (flatmapLinear₁ z) with side₁ z
      StStream≅.stream (flatmapLinear₁ z) | c with step z
      StStream≅.stream (flatmapLinear₁ z) | c | just x , z' with pred z' | inspect pred z'
      StStream≅.stream (flatmapLinear₁ z) | c | just x , z' | false | [ prf ] rewrite prf = eqnil
      StStream≅.stream (flatmapLinear₁ z) | c | just x , z' | true  | [ prf ] rewrite prf = eqskip (flatmapLinear₁Lemma z' x z' (zi z' x))
      StStream≅.stream (flatmapLinear₁ z) | c | nothing , z' with pred z' | inspect pred z'
      StStream≅.stream (flatmapLinear₁ z) | c | nothing , z' | false | [ prf ] rewrite prf = eqnil
      StStream≅.stream (flatmapLinear₁ z) | () | nothing , z' | true | [ prf ]

  module _ {A B Z Zp : Set} (step : Z → Maybe A × Z) (pred : Z → Bool) (stepi : Z → A → (Zp × Z) → Maybe B × (Zp × Z)) (zi : Z → A → Zp × Z) (predi : Z → A → (Zp × Z) → Bool)
                            (side₁ : (z : Z) → step z |> λ { (just x , z') → ⊤ ; (nothing , z') → pred z' |> λ { false → ⊤ ; true → ⊥ } })
                            (side₂ : (x : A) (z₀ : Z) (z : Zp × Z) → stepi z₀ x z |> λ { (just x , z') → ⊤ ; (nothing , z') → predi z₀ x z' |> λ { false → ⊤ ; true → ⊥ } }) where
    private
      step' : Maybe (A × Z × Zp × Z) × Z → Maybe B × Maybe (A × Z × Zp × Z) × Z
      step' (just (a , z₀ , zp , z) , z₀') with side₂ a z₀ (zp , z)
      step' (just (a , z₀ , zp , z) , z₀') | c with stepi z₀ a (zp , z)
      step' (just (a , z₀ , zp , z) , z₀') | c | just b , (zp' , z') with predi z₀ a (zp' , z')
      step' (just (a , z₀ , zp , z) , z₀') | c | just b , zp' , z' | false = (nothing , (nothing  , z₀'))
      step' (just (a , z₀ , zp , z) , z₀') | c | just b , zp' , z' | true = (just b) , ((just (a , (z₀ , (zp' , z')))) , z')
      step' (just (a , z₀ , zp , z) , z₀') | c | nothing , (zp' , z') with predi z₀ a (zp' , z')
      step' (just (a , z₀ , zp , z) , z₀') | c | nothing , zp' , z' | false = nothing , (nothing  , z₀')
      step' (nothing , z) with side₁ z
      step' (nothing , z) | c with step z 
      step' (nothing , z) | c | just x , z' with pred z'
      step' (nothing , z) | c | just x , z' | false = (nothing , (nothing , z'))
      step' (nothing , z) | c | just x , z' | true  = let (zp , z'') = zi z' x in
                                                      nothing , (just (x , (z' , (zp , z'')))  , z')
      step' (nothing , z) | c  | nothing , z' with pred z' 
      step' (nothing , z) | c  | nothing , z' | false = (nothing , (nothing , z')) 
      step' (nothing , z) | () | nothing , z' | true

    mutual
      flatmapLinear₂Lemma : ∀ (z₀ : Z) (a : A) (z : Z) ((zp' , z') : Zp × Z) →
                   StStream≅ (guard pred (StStreamFunctions.flatmapInnerSt (λ z₁ x₁ → abstractSt (guard (predi z₁ x₁) (unroll (stepi z₁ x₁) (zi z₁ x₁)))) z
                                                               (λ z₁ → guard pred (unroll step z₁))
                                                               (abstractSt (guard (predi z₀ a) (unroll (stepi z₀ a) (zp' , z'))))))
                             (abstractSt (guard (λ { (_ , z) → pred z }) (unroll step' (just (a , z₀ , zp' , z') , z))))
      StStream≅.stream (flatmapLinear₂Lemma z₀ a z (zp' , z')) with side₂ a z₀ (zp' , z')
      StStream≅.stream (flatmapLinear₂Lemma z₀ a z (zp' , z')) | c with stepi z₀ a (zp' , z')
      StStream≅.stream (flatmapLinear₂Lemma z₀ a z (zp' , z')) | c | just b , zp'' , z'' with predi z₀ a (zp'' , z'')
      StStream≅.stream (flatmapLinear₂Lemma z₀ a z (zp' , z')) | c | just b , zp'' , z'' | false with pred z
      StStream≅.stream (flatmapLinear₂Lemma z₀ a z (zp' , z')) | c | just b , zp'' , z'' | false | false = eqnil
      StStream≅.stream (flatmapLinear₂Lemma z₀ a z (zp' , z')) | c | just b , zp'' , z'' | false | true = eqskip (flatmapLinear₂ z)
      StStream≅.stream (flatmapLinear₂Lemma z₀ a z (zp' , z')) | c | just b , zp'' , z'' | true with pred z''
      StStream≅.stream (flatmapLinear₂Lemma z₀ a z (zp' , z')) | c | just b , zp'' , z'' | true | false = eqnil
      StStream≅.stream (flatmapLinear₂Lemma z₀ a z (zp' , z')) | c | just b , zp'' , z'' | true | true = eqcons refl (flatmapLinear₂Lemma z₀ a z'' (zp'' , z''))
      StStream≅.stream (flatmapLinear₂Lemma z₀ a z (zp' , z')) | c | nothing , zp'' , z'' with predi z₀ a (zp'' , z'')
      StStream≅.stream (flatmapLinear₂Lemma z₀ a z (zp' , z')) | c | nothing , zp'' , z'' | false with pred z
      StStream≅.stream (flatmapLinear₂Lemma z₀ a z (zp' , z')) | c | nothing , zp'' , z'' | false | false = eqnil
      StStream≅.stream (flatmapLinear₂Lemma z₀ a z (zp' , z')) | c | nothing , zp'' , z'' | false | true = eqskip (flatmapLinear₂ z)


      flatmapLinear₂ : ∀ (z : Z) → StStream≅ (unroll step z |> guard pred |> flatmapSt (λ z x → unroll (stepi z x) (zi z x) |> guard (predi z x) |> abstractSt) |> guard (λ { z → pred z }))
                                             (unroll step' (nothing , z) |> guard (λ { (_ , z) → pred z }) |> abstractSt)
      StStream≅.stream (flatmapLinear₂ z) with side₁ z
      StStream≅.stream (flatmapLinear₂ z) | c with step z
      StStream≅.stream (flatmapLinear₂ z) | c | just a , z' with pred z' | inspect pred z'
      StStream≅.stream (flatmapLinear₂ z) | c | just a , z' | false | [ prf ] rewrite prf = eqnil
      StStream≅.stream (flatmapLinear₂ z) | c | just a , z' | true | [ prf ] rewrite prf = eqskip (flatmapLinear₂Lemma z' a z' (zi z' a))
      StStream≅.stream (flatmapLinear₂ z) | c | nothing , z' with pred z' | inspect pred z'
      StStream≅.stream (flatmapLinear₂ z) | c | nothing , z' | false | [ prf ] rewrite prf = eqnil
      


