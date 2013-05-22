{-# language OverloadedStrings #-}

module Test.T11 where


import JPrelude


main :: JIO ()
main = do
    jputStrLn "a"
    jputStrLn $ error "expected error"
