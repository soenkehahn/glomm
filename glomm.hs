

import Language.Core.ParseGlue
import Language.Core.Parser
import Language.Core.Core as Core
import System.Environment
import Text.Show.Pretty
import Text.Printf
-- ~ import Language.Sunroof
-- ~ import Language.Sunroof.JS.Browser (alert)
-- ~ import Language.Sunroof.JavaScript as JS
import Data.Default


main = do
    [file] <- getArgs
    code <- readFile file
    let eModul = parse code 0
    (prelude, rts_) <- rts
    case eModul of
        OkP modul -> do
--             putStrLn $ ppShow modul
--             putStrLn "\n==========\n"
            putStrLn (toJS prelude rts_ modul)
        FailP err -> error err

toJS :: String -> String -> Module -> String
toJS prelude rts (Module _ _ vdefs) =
    let stmts = map (vdefgToJS) vdefs
    in unlines (prelude : stmts ++ rts : [])

type Stmt = String

vdefgToJS :: Vdefg -> Stmt
vdefgToJS (Nonrec vdef) = vdefToJS vdef
vdefgToJS (Core.Rec vdefs) = concat $ map vdefToJS vdefs

vdefToJS :: Vdef -> Stmt
vdefToJS (Vdef ((_, name), typ, exp)) =
    "var " ++ name ++ " = " ++ expToJS exp ++ "\n"

type Expr = String

expToJS :: Exp -> Expr
expToJS (Lam (Vb (var, _)) exp) =
    "function (" ++ var ++ ") { return (" ++ expToJS exp ++ ")}"
expToJS (Lam (Tb (t, k)) exp) = expToJS exp
    -- ~ error $ show ("tblam", t, k, exp)
expToJS (App f x) = "(" ++ expToJS f ++ ")(" ++ expToJS x ++ ")"
expToJS (Core.Var (_, name)) = name
expToJS (Core.Lit (Literal coreLit _)) = coreLitToJS coreLit
expToJS (Core.Dcon (t, name)) =
    "konstruktor(\"" ++ name ++ "\")"
expToJS (Core.Appt exp t) = "patError()" -- error $ show ("appt", exp, t)
expToJS (Core.Let x y) = error $ show ("let", x)
expToJS (Core.Cast x y) = error $ show ("cast", x)
expToJS (Core.Note x y) = error $ show ("note", x)
expToJS (Core.External x y) = error $ show ("ext", x)
expToJS (Core.Case scrutinee y z alts) =
    printf "function () {\nvar scrutinee = %s\n%s\nreturn result}()" (expToJS scrutinee) (altsToIfs alts)

-- ~ expToJS e = error $ show ("expp", e)

altsToIfs :: [Alt] -> Expr
altsToIfs (Acon (_, konsName) _ binds exp : r) =
    printf "if (scrutinee.namee === \"%s\") {\n%s\n%s\n} else %s"
        konsName
        (mkBinds binds)
        ("result = " ++ expToJS exp)
        (altsToIfs r)
  where
    mkBinds :: [Vbind] -> String
    mkBinds = concat . map inner . zip [0..]
    inner :: (Int, Vbind) -> String
    inner (i, (var, _)) = printf "var %s = scrutinee.argss[%i]\n" var i
altsToIfs (Alit lit exp : r) = error $ show ("lit", lit, exp)
altsToIfs (Adefault exp : []) = "{" ++ expToJS exp ++ "}"
altsToIfs [] = "throw paterror"

coreLitToJS :: CoreLit -> Expr
coreLitToJS (Lint n) = (show n)
coreLitToJS (Lstring s) = "\"" ++ s ++ "\""


-- * RTS

rts :: IO (String, String)
rts = do
    prelude <- readFile "pre.js"
    rts <- readFile "post.js"
    return (prelude, rts)
 
