{-# language MagicHash #-}

module JPrelude where


import GHC.Prim
import Data.String


data JString = JString Addr#

instance IsString JString where
    fromString s = ffi1 "stringToAddr" s

ffi1 = error "magic ffi1"
ffi2 = error "magic ffi2"

jputStrLn :: JString -> JIO ()
jputStrLn s = JIO (ffi2 "jPrim.jputStrLn" s)

type JRealWorld = JString

data JIO o = JIO (JRealWorld -> (o, JRealWorld))

instance Monad JIO where
    return = error "return"
    fail = error "fail"
    JIO a >>= fb = JIO $ \ realWorld ->
        let (x, realWorld2) = a realWorld
        -- careful with realWorld2, it's the only one we got!
            (JIO b) = fb x
        in b realWorld2

alert :: JString -> JIO ()
alert s = JIO (ffi2 "jPrim.alert" s)
