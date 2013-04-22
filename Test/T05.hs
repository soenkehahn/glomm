{-# language NoImplicitPrelude #-}

module Test.T05 (main) where


-- ~ import GHC.Num
-- ~ 
-- ~ 
-- ~ main :: Integer
-- ~ main = fac 6 -- Succ (Succ (Succ Zero)) * Succ (Succ (Succ (Succ Zero)))
-- ~ 
-- ~ fac 1 = 1
-- ~ fac n = n * fac (n - 1)


main :: Nat
main = S (S (S (S Z))) * S (S (S Z))

data Nat = Z | S Nat

Z + b = b
S a + b = a + S b

Z * b = Z
S a * b = b + (a * b)
