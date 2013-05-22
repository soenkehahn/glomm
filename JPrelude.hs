{-# language MagicHash #-}

module JPrelude where


import GHC.Prim
import Data.String


data JString = JString Addr#

instance IsString JString where
    fromString s = ffi1 "stringToAddr" s

ffi1 = error "magic"
ffi2 = error "magic"

consoleLog :: JString -> JIO ()
consoleLog s = JIO (ffi2 "jPrim.consoleLog" s)

data JIO o = JIO (JRealWorld -> (o, JRealWorld))

type JRealWorld = JString
