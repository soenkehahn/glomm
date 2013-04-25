

import Control.Arrow ((>>>))
import Data.List
import Development.Shake
import Development.Shake.FilePath
import System.Environment
import System.Process
import Control.Applicative ((<$>))
import Data.List

import Translate


main = shakeArgs shakeOptions{shakeFiles = "_make/", shakeProgress = progressSimple} $ do
    phony "clean" $ removeFilesAfter "_make" ["//*"]

    ["_make//*.hs.hcr", "_make//*.hi"] *>> \ [coreFile, hiFile] -> do
        let hsFile = dropDirectory1 $ dropExtension coreFile
        hsImports <- readFileLines ("_make" </> hsFile <.> "directImports")
        let importHiFiles = map (\ f -> "_make" </> replaceExtension f ".hi") hsImports
        need (hsFile : importHiFiles)
        system' "ghc" $
            hsFile :
            "-fext-core" :
            "-S" :
            "-outputdir" : "_make" :
            "-i_make" :
            "-hide-all-packages" :
            "-package" : "base" :
            []
        system' "rm" [replaceExtension hsFile ".s"]
        system' "mv" $
            (replaceExtension hsFile ".hcr") :
            coreFile :
            []

    "_make//*.hs.hcr.js" *> \ jsFile -> do
        need ["pre.js", "ghcPrim.js", "post.js"]
        need $
            "_make/ghc-prim.package.js.opt" :
            "_make/base.package.js.opt" :
            "_make/integer-simple.package.js.opt" :
            []
        let coreFile = dropExtension jsFile
        hsImports <- readFileLines (dropExtension coreFile <.> "transitiveImports")
        let importCoreFiles = map (\ f -> "_make" </> f <.> ".hcr") hsImports
        need (coreFile : importCoreFiles)
        liftIO $ putStrLn ("compiling " ++ show (coreFile : importCoreFiles))
        liftIO $ compileFiles (MainModule coreFile importCoreFiles) jsFile

    "_make/*.package.js" *> \ jsFile -> do
        let package = dropDirectory1 $ dropExtension $ dropExtension jsFile
        modules <-
            map (\ f -> package </> replaceExtension f ".hcr") <$>
            filter (not . ("#" `isPrefixOf`)) <$>
            readFileLines (package </> "modules")
        need modules
        liftIO $ putStrLn (package ++ ": " ++ show modules)
        liftIO $ compileFiles (Package package modules) jsFile

    "_make//*.hs.transitiveImports" *> \ importsFile -> do
        directImports <- readFileLines (replaceExtension importsFile ".directImports")
        transitiveImports <- concat <$> mapM (\ f -> readFileLines ("_make" </> f <.> "transitiveImports")) directImports
        liftIO $ writeFile importsFile $ unlines (nub (directImports ++ transitiveImports))

    "_make//*.hs.directImports" *> \ importsFile -> do
        let hsFile = dropDirectory1 $ dropExtension importsFile
            ghcMakeFile = importsFile <.> "ghcMakeFile"
            oFile = replaceExtension hsFile ".o"
        system' "ghc" $
            "-M" : "-dep-makefile" : ghcMakeFile :
            "-hide-all-packages" :
            "-package" : "ghc-prim" :
            "-package" : "base" :
            hsFile :
            []
        directImports <- parseImports oFile hsFile <$> readFile' ghcMakeFile
        liftIO $ writeFile importsFile $ unlines directImports

    "//*.js.opt" *> \ optFile -> do
        let jsFile = dropExtension optFile
        need [jsFile]
        -- google's closure compiler
        system' "closure" $
            "--js" : jsFile :
            "--js_output_file" : optFile :
            []

    return ()


parseImports :: FilePath -> FilePath -> String -> [FilePath]
parseImports oFile hsFile =
    lines >>>
    filter (oFile `isPrefixOf`) >>>
    filter (not . (hsFile `isSuffixOf`)) >>>
    map (words >>>
         last >>>
         flip replaceExtension ".hs")
