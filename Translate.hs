
{-# language ScopedTypeVariables #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language DeriveTraversable #-}
{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}


module Translate where


import Control.Applicative
import Control.Exception (assert)
import Control.Monad (zipWithM, when)
import Data.Boolean
import Data.Char
import Data.Default
import Data.Foldable
import Data.List (intercalate)
import Data.List (sortBy)
import Data.Map as Map (Map, insert, lookup, keys, (!), fromList)
import Data.Maybe
import Data.Monoid
import Data.Ratio
import Data.Traversable
import Debug.Trace
import Language.Core.Core as Core
import Language.Core.ParseGlue
import Language.Core.Parser
import Language.Sunroof as JS
import Prelude hiding (mapM, foldl, elem)
import System.Environment
import Text.Printf
import Text.Show.Pretty


data Modules a = Modules {
    mainModule :: a,
    imports :: [a]
  }
    deriving (Functor, Traversable, Foldable, Show)


rts :: IO (String, String)
rts = do
    ghcPrim <- readFile "ghcPrim.js"
    prelude <- readFile "pre.js"
    rts <- readFile "post.js"
    return (prelude ++ ghcPrim, rts)


compileFiles :: Modules FilePath -> FilePath -> IO ()
compileFiles inputFiles outputFile = do
    code <- mapM readFile inputFiles
    let eModules = mapM (\ c -> parse c 0) code
    (pre, post) <- rts
    case eModules of
        OkP modules -> do
            jsCode <- toJS pre post modules
            writeFile outputFile jsCode
        FailP err -> error ("core parse error: " ++ err)

instance Monad ParseResult where
    OkP a >>= b = b a
    FailP err >>= b = FailP err
    return = OkP

toJS :: String -> String -> Modules Module -> IO String
toJS pre post modules = do
    let getVdefs (Module _ _ vdefs) = vdefs
        vdefgs = mconcat $ fmap getVdefs (toList modules)
        (Module anName _ _) = mainModule modules
        entryPoint = (Just anName, "main")
    jsCode <- sunroofCompileJSA def "sunroofMain" $ do
        c <- emptyContext
        topLevel :: Context <- letContext (Core.Rec $ flattenBinds vdefgs) c
        -- ~ error $ show topLevel
        return (topLevel Map.! entryPoint)
    return $ unlines (pre : jsCode : post : [])

emptyContext :: JSA Context
emptyContext = do
    patError <- whnf $ object "patError"
    let patErrorName = (Just (M (P "base", ["Control", "Exception"], "Base")), "patError")
    return $ Map.fromList $
        (patErrorName, patError) :
        []


type QName = Qual Var

type Context = Map QName Term

type GenerateTerm = Context -> JSA Term

type Term = JSObject


qnameToJSAttr :: QName -> String
qnameToJSAttr (Nothing, v) =
    "l_" ++ v
qnameToJSAttr q@(Just (M (P package, parentModules, mod)), v) =
    "g_" ++
    intercalate "_" (package : parentModules ++ mod : v : [])

-- | Creates a term for a term that is already in whnf.
--   (More efficient than quote.)
whnf :: JSObject -> JSA Term
whnf x = apply (cast $ object "glommFromWhnf") x

-- | Quotes a value (defers evaluation).
quote :: JSA JSObject -> JSA Term
quote codeGen = do
    f <- function $ \ () -> codeGen
    apply (cast $ object "glommQuoted") f


letContext :: Vdefg -> Context -> JSA Context
letContext (Nonrec vdef) context =
    comment "could be done more efficiently" >>
    letContext (Core.Rec [vdef]) context
letContext (Core.Rec vdefs) parentContext = do
    outerScope <- fixJS $ \ recScope -> do
        innerContext <- mkContextFromArray vdefs recScope
        values <- forM vdefs $ \ (Vdef ((q, name), _, rhs)) -> do
            comment ("letrec assignment to '" ++ name ++ "' ")
            r <- generateTerm rhs innerContext
            comment ("end " ++ name)
            return r
        scope <- new "Object" ()
        forM_ (zip (map vdefName vdefs) values) $ \ (name, value) ->
            scope # attr (qnameToJSAttr name) := value
        return scope
    mkContextFromArray vdefs outerScope
  where
    mkContextFromArray :: [Vdef] -> JSObject -> JSA Context
    mkContextFromArray vdefs scope =
        foldl (\ c (name, v) -> insert name v c) parentContext <$>
        zipWithM (inner scope) [0..] vdefs
    inner :: JSObject -> Integer -> Vdef -> JSA (QName, Term)
    inner scope i vdef = do
        t <- quote $ return (scope JS.! attr (qnameToJSAttr (vdefName vdef)))
        return (vdefName vdef, t)


generateTerm :: Exp -> GenerateTerm
generateTerm (Lam (Vb (var, _)) exp) context = do
    fun <- function (\ a -> generateTerm exp (insert (Nothing, var) a context))
    whnf (cast fun)
generateTerm (Lam (Tb (t, k)) exp) context = do
    throw "tblam"
    comment ("tblam: " ++ show (t, k))
    generateTerm exp context
generateTerm (App f x) context = do
    fun <- generateTerm f context
    arg <- generateTerm x context
    apply (cast $ object "glommApply") (fun, arg)
generateTerm (Core.Var qname) context =
    maybe
        (comment ("var lookup error: " ++ show qname ++ "\n" ++ ppShow (keys context)) >> return (object (qnameToJSAttr qname)))
        (return)
        (Map.lookup qname context)
generateTerm (Core.Lit (Literal coreLit typ)) context = do
    comment ("literal " ++ show coreLit)
    coreLitToJS coreLit typ
generateTerm (Core.Dcon (t, name)) _ =
    whnf (object ("glommConstructorFunction(\"" ++ name ++ "\")"))
generateTerm (Core.Appt exp t) context = do
    comment ("appt " ++ show t)
    generateTerm exp context
generateTerm (Core.Let vdefg exp) context = do
    newContext <- letContext vdefg context
    generateTerm exp newContext
generateTerm (Core.Cast exp y) context = do
    comment ("cast to " ++ show y)
    generateTerm exp context
generateTerm (Core.Note x y) _ = error $ show ("note", x)
generateTerm (Core.External externalVar typ) context = throw ("external call: " ++ externalVar)
generateTerm (Core.Case scrutineeJS scrutineeBind typ alts) context = do
    scrutinee <- generateTerm scrutineeJS context
    rhs <- function $ \ scrutineeInWhnf ->
        altsToIfs scrutineeInWhnf (sortAlts alts) (insert (Nothing, fst scrutineeBind) scrutineeInWhnf context)
    apply (cast $ object "glommCast") (scrutinee, rhs)
generateTerm exp _ = error $ show ("exp", exp)

sortAlts :: [Alt] -> [Alt]
sortAlts = sortBy inner
  where
    inner (Adefault _) (Adefault _) = EQ
    inner (Adefault _) x = GT
    inner x (Adefault _) = LT
    inner a b = EQ

altsToIfs :: JSObject -> [Alt] -> GenerateTerm
altsToIfs scrutinee (Acon (_, consName) tbinds binds exp : r) context = do
    comment ("tbinds: " ++ show tbinds)
    comment ("vbinds: " ++ show binds)
    patternBinds :: [(QName, Term)] <- forM (zip [0..] binds) $ \ (i, (var, _)) -> do
        let args :: JSArray JSObject = (scrutinee JS.! attr "value" :: JSObject) JS.! (label "glommConstructorArgs")
        return ((Nothing, var), args JS.! index (cast (object (show i))))
    let contextWithPatternBinds = foldl (\ m (n, t) -> insert n t m) context patternBinds
    ifB (((scrutinee JS.! attr "value" :: JSObject) JS.! attr "glommConstructorName") ==* string consName)
        -- then
        (generateTerm exp contextWithPatternBinds)
        -- else
        (altsToIfs scrutinee r context)
altsToIfs scrutinee (Alit (Literal lit typ) exp : r) context = do
    litTerm <- coreLitToJS lit typ
    ifB ((scrutinee JS.! attr "value" :: JSObject) ==* (litTerm JS.! attr "value"))
        -- then
        (generateTerm exp context)
        -- else
        (altsToIfs scrutinee r context)
altsToIfs scrutinee (Adefault exp : []) context = do
    comment "Adefault"
    generateTerm exp context
altsToIfs scrutinee (Adefault exp : r) context = error "Adefault should always come last."
altsToIfs _ [] _ = throw "pattern matching failure"

-- | Returns a litaral as a Term in whnf.
coreLitToJS :: CoreLit -> Ty -> JSA Term
coreLitToJS (Lint n) typ | show typ `elem` [
        "ghczmprim:GHCziPrim.Intzh",
        "ghczmprim:GHCziPrim.Wordzh",
        "ghczmprim:GHCziPrim.Charzh"]
    = whnf $ object $ show n
coreLitToJS (Lint n) typ | show typ == "integerzmgmp:GHCziIntegerziType.Integer"
    = do
        comment "newInt"
        integerToJSTerm n
coreLitToJS (Lstring s) typ | show typ `elem` [
        "ghczmprim:GHCziPrim.Addrzh"]
    = whnf $ object (show s)
coreLitToJS (Lchar c) typ | show typ == "ghczmprim:GHCziPrim.Charzh"
    = whnf $ object $ show (ord c)
coreLitToJS (Lrational r) typ | show typ `elem` [
        "ghczmprim:GHCziPrim.Doublezh",
        "ghczmprim:GHCziPrim.Floatzh"]
    = whnf $ object $ printf "(%i / %i)" (numerator r) (denominator r)
coreLitToJS x typ = error $ show ("coreLit", x, typ, cons x)
  where
    cons (Lint n) = "Lint"
    cons (Lchar c) = "Lchar"
    cons (Lstring s) = "Lstring"
    cons (Lrational r) = "Lrational"

integerToJSTerm :: Integer -> JSA Term
integerToJSTerm 0 = cons "Naught"
integerToJSTerm n | n > 0 && n < 127 = do
    digit <- whnf $ object (show n)
    none <- cons "None"
    let digits = none
    some <- cons "Some"
    someDigit <- app some digit
    digits <- app someDigit digits
    positive <- cons "Positive"
    app positive digits
integerToJSTerm n = error ("integerToJSTerm: " ++ show n)

cons :: String -> JSA Term
cons n = whnf $ object ("glommConstructorFunction(\"" ++ n ++ "\")")
app :: Term -> Term -> JSA Term
app fun arg = apply (cast $ object "glommApply") (fun, arg)


vdefName :: Vdef -> QName
vdefName (Vdef (qname, _, _)) = qname

throw :: String -> JSA Term
throw err = quote $ return $ object ("(function () {throw \"" ++ err ++ "\";})()")
