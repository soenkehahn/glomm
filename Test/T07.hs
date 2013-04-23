{-# language NoImplicitPrelude #-}

module Test.T07 (main) where


main :: Nat
main = fac (S (S (S (S Z))))

fac :: Nat -> Nat
fac Z = S Z
fac (S x) = S x * fac x

data Nat = Z | S Nat

Z * b = Z
S a * b = b + (a * b)

Z + b = b
S a + b = a + S b
