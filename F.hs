{-# language NoImplicitPrelude #-}

module F where


import Prelude (error, Bool(..), error)


data Nat = Zero | Succ Nat

(+) :: Nat -> Nat -> Nat
a + Zero = a
a + Succ b = Succ a + b

(*) :: Nat -> Nat -> Nat
a * b | b == Zero = Zero
a * b = a + (a * pred b)

(==) :: Nat -> Nat -> Bool
Zero == Zero = True
Succ a == Succ b = a == b
_ == _ = False

pred (Succ a) = a
pred x = error "geht nicht"

main :: Nat
main = Succ (Succ (Succ Zero)) * Succ (Succ (Succ (Succ Zero)))
