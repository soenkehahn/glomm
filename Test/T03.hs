{-# language NoImplicitPrelude #-}

module Test.T03 (main) where


-- ~ import GHC.Num
-- ~ 
-- ~ 
-- ~ main :: Integer
-- ~ main = fac 6 -- Succ (Succ (Succ Zero)) * Succ (Succ (Succ (Succ Zero)))
-- ~ 
-- ~ fac 1 = 1
-- ~ fac n = n * fac (n - 1)


main :: Bool
main = rec True

rec False = False
rec True = rec False

data Bool = True | False
