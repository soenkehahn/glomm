
{-# language ScopedTypeVariables #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}


module Translate where


import Data.Boolean
import Control.Monad (zipWithM, when)
import Control.Applicative
import Data.Default
import Data.Foldable (forM_)
import Data.Map as Map (Map, insert, lookup, keys, (!), fromList)
import Data.Monoid
import Data.Ratio
import Data.Traversable (forM)
import Debug.Trace
import Language.Core.Core as Core
import Language.Core.ParseGlue
import Language.Core.Parser
import Language.Sunroof as JS
import Prelude
import System.Environment
import Text.Printf
import Text.Show.Pretty


compileFiles :: [FilePath] -> FilePath -> IO ()
compileFiles inputFiles outputFile = do
    print inputFiles
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

toJS :: String -> String -> [Module] -> IO String
toJS pre post modules = do
    let getVdefs (Module _ _ vdefs) = vdefs
        vdefgs = concat $ fmap getVdefs modules
    jsCode <- sunroofCompileJSA def "sunroofMain" $ do
        c <- emptyContext
        topLevel :: Context <- letContext (Core.Rec $ flattenBinds vdefgs) c
        return (topLevel Map.! "main")
    return $ unlines (pre : jsCode : post : [])

emptyContext :: JSA Context
emptyContext = do
    patError <- whnf $ object "patError"
    return $ Map.fromList $
        ("patError", patError) :
        []



type Context = Map String Term

type GenerateTerm = Context -> JSA Term

type Term = JSObject


-- | Creates a term for a term that is already in whnf.
--   (More efficient than quote.)
whnf :: JSObject -> JSA Term
whnf x = apply (cast $ object "glommFromWhnf") x

-- | Quotes a value (defers evaluation).
quote :: JSA JSObject -> JSA Term
quote codeGen = do
    f <- function $ \ () -> codeGen
    apply (cast $ object "glommQuoted") f

-- | Unquotes a value.
forceWhnf :: Term -> JSA JSObject
forceWhnf t = t # invoke "forceWhnf" ()


generateTerm :: Exp -> GenerateTerm
generateTerm (Lam (Vb (var, _)) exp) context = do
    fun <- function (\ a -> generateTerm exp (insert var a context))
    whnf (cast fun)
generateTerm (Lam (Tb (t, k)) exp) context = do
    error "tblam"
    comment ("tblam: " ++ show (t, k))
    generateTerm exp context
generateTerm (App f x) context = do
    fun <- generateTerm f context
    arg <- generateTerm x context
    apply (cast $ object "glommApply") (fun, arg)
generateTerm (Core.Var (_, name)) context =
    maybe
        (error ("var lookup error: " ++ name ++ "\n" ++ ppShow (keys context)))
        (return)
        (Map.lookup name context)
generateTerm (Core.Lit (Literal coreLit _)) context = do
    comment ("literal " ++ show coreLit)
    coreLitToJS coreLit context
generateTerm (Core.Dcon (t, name)) _ =
    whnf (object ("glommConstructorFunction(\"" ++ name ++ "\")"))
generateTerm (Core.Appt exp t) context = do
    comment ("appt " ++ show t)
    generateTerm exp context
generateTerm (Core.Let vdefg exp) context = do
    newContext <- letContext vdefg context
    generateTerm exp newContext
generateTerm (Core.Cast exp y) context = error "cast" -- generateTerm exp context
generateTerm (Core.Note x y) _ = error $ show ("note", x)
-- ~ generateTerm (Core.External externalVar typ) = externalVar
generateTerm (Core.Case scrutineeJS y z alts) context = do
    scrutineeLazy <- generateTerm scrutineeJS context
    quote $ do
        comment "force scrutinee"
        scrutinee <- forceWhnf scrutineeLazy
        altsToIfs scrutinee alts context
generateTerm exp _ = error $ show ("exp", exp)

altsToIfs :: JSObject -> [Alt] -> GenerateTerm
altsToIfs scrutinee (Acon (_, consName) _ binds exp : r) context = do
    patternBinds :: [(String, Term)] <- forM (zip [0..] binds) $ \ (i, (var, _)) -> do
        let args :: JSArray JSObject = (scrutinee JS.! attr "value" :: JSObject) JS.! (label "glommConstructorArgs")
        return (var, args JS.! index (cast (object (show i))))
    let contextWithPatternBinds = foldl (\ m (n, t) -> insert n t m) context patternBinds
    ifB (((scrutinee JS.! attr "value" :: JSObject) JS.! attr "glommConstructorName") ==* string consName)
        -- then
        (generateTerm exp contextWithPatternBinds)
        -- else
        (altsToIfs scrutinee r context)
-- ~ altsToIfs (Alit lit exp : r) = error $ show ("lit", lit, exp)
-- ~ altsToIfs (Adefault exp : r) = "{" ++ generateTerm exp ++ "}"
-- ~ altsToIfs (Adefault exp : r) = error "ju"
altsToIfs _ [] _ = throw "pattern matching failure"
altsToIfs scrutinee x context = error $ show ("altsToIfs", x)

coreLitToJS :: CoreLit -> GenerateTerm
-- ~ coreLitToJS (Lint n) _ = quote $ object $ show n
coreLitToJS (Lstring s) _ = whnf $ object ("\"" ++ s ++ "\"")
-- ~ coreLitToJS (Lchar c) = error "char"
-- ~ coreLitToJS (Lrational r) = printf "(%i, %i)" (numerator r) (denominator r)
coreLitToJS x _ = error $ show ("coreLit", x)

letContext :: Vdefg -> Context -> JSA Context
letContext (Nonrec vdef) parentContext = do
    error "let"
    -- ~ x <- generateTerm rhs parentContext
    -- ~ return $ insert name x parentContext
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
            scope # attr name := value
        return scope
    mkContextFromArray vdefs outerScope
  where
    mkContextFromArray :: [Vdef] -> JSObject -> JSA Context
    mkContextFromArray vdefs scope =
        foldl (\ c (name, v) -> insert name v c) parentContext <$>
        zipWithM (inner scope) [0..] vdefs
    inner :: JSObject -> Integer -> Vdef -> JSA (String, Term)
    inner scope i vdef = do
        t <- quote $ return (scope JS.! attr (vdefName vdef))
        return (vdefName vdef, t)

vdefName :: Vdef -> String
vdefName (Vdef ((q, n), _, _)) = n

throw :: String -> JSA Term
throw err = quote $ return $ object ("(function () {throw \"" ++ err ++ "\";})()")

-- * RTS

rts :: IO (String, String)
rts = do
    prelude <- readFile "pre.js"
    rts <- readFile "post.js"
    return (prelude, rts)
