

import Control.Arrow ((>>>))
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
            []
        system' "mv" $
            (replaceExtension hsFile ".hcr") :
            coreFile :
            []

    "_make//*.hs.hcr.js" *> \ jsFile -> do
        let coreFile = dropExtension jsFile
        hsImports <- readFileLines (dropExtension coreFile <.> "transitiveImports")
        let importCoreFiles = map (\ f -> "_make" </> f <.> ".hcr") hsImports
        need (coreFile : importCoreFiles)
        liftIO $ putStrLn ("compiling " ++ show (coreFile : importCoreFiles))
        liftIO $ compileFiles (Modules coreFile importCoreFiles) jsFile

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
            "-include-package-deps" :
            hsFile :
            []
        directImports <- parseImports oFile hsFile <$> readFile' ghcMakeFile
        liftIO $ writeFile importsFile $ unlines directImports

    return ()


parseImports :: FilePath -> FilePath -> String -> [FilePath]
parseImports oFile hsFile =
    lines >>>
    filter (oFile `isPrefixOf`) >>>
    filter (not . (hsFile `isSuffixOf`)) >>>
    map (words >>>
         last >>>
         flip replaceExtension ".hs")
