{-# language MagicHash, GADTs #-}

module JPrelude where


import GHC.Prim
import Data.String
import Data.Monoid


data JString = JString Addr#

instance Eq JString where
    a == b = ffi2 "jPrim.jstringEquals" a b

instance IsString JString where
    fromString s = ffi1 "stringToAddr" s

instance Monoid JString where
    mappend = ffi2 "jPrim.jstringAppend"
    mempty = error "mempty"

ffi1 = error "magic ffi1"
ffi2 = error "magic ffi2"

jputStrLn :: JString -> JIO ()
jputStrLn s = JIO (ffi2 "jPrim.jputStrLn" s)

type JRealWorld = JString

data JIO o where
    JIO :: (JRealWorld -> (o, JRealWorld)) -> JIO o
    JIOBind :: JIO a -> (a -> JIO b) -> JIO b

instance Monad JIO where
    return a = JIO $ \ rw -> (a, rw)
    fail = error "fail"
    (>>=) = JIOBind

alert :: JString -> JIO ()
alert s = JIO (ffi2 "jPrim.alert" s)

prompt :: JString -> JIO JString
prompt s = JIO (ffi2 "jPrim.prompt" s)
