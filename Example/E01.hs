{-# language OverloadedStrings #-}

module Example.E01 where


import Data.Monoid

import JPrelude


main :: JIO()
main = do
    jputStrLn "started..."
    loop

loop = do
    input <- prompt "please type your name"
    if input == "" then do
        jputStrLn "empty name not allowed"
        loop
      else
        jputStrLn ("Hello " <> input <> "!")
