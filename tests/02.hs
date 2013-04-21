{-# language NoImplicitPrelude #-}

module B where

main :: Bool
main = not False

not False = True

data Bool = False | True

