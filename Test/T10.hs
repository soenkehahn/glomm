{-# language OverloadedStrings #-}

module Test.T10 where


import JPrelude


main :: JIO ()
main = do
    alert "alert"
    jputStrLn "log"

_main :: (JString, JString)
_main = ("huhu", "haha")
