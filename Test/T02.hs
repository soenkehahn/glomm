{-# language NoImplicitPrelude #-}

module Test.T02 where

main :: Bool
main = not False

not False = True

data Bool = False | True

