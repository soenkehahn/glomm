{-# language MagicHash #-}

module Test.T08 (main) where


main :: String
main = "huhu" -- if bigger 10 "huhu" then error "bug" else "huhu"

-- ~ bigger n (a : r) = if n <= 0 then True else bigger (pred n) r
