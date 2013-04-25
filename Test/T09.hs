

module Test.T09 (main) where

main :: Nat
main = 15

data Nat = Zero | Succ Nat
  deriving Show

instance Num Nat where
    fromInteger x | x < 0 = error "neg Nat"
    fromInteger 0 = Zero
    fromInteger n = Succ $ fromInteger $ pred n
