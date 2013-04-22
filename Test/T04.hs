{-# language NoImplicitPrelude #-}

module Test.T04 (main) where


-- ~ import GHC.Num
-- ~ 
-- ~ 
-- ~ main :: Integer
-- ~ main = fac 6 -- Succ (Succ (Succ Zero)) * Succ (Succ (Succ (Succ Zero)))
-- ~ 
-- ~ fac 1 = 1
-- ~ fac n = n * fac (n - 1)


main :: Bool
main = isOdd (S (S (S Z)))

isOdd :: Nat -> Bool
isOdd Z = False
isOdd (S a) = not (isOdd a)

not False = True
not True = False


data Nat = Z | S Nat
data Bool = True | False
