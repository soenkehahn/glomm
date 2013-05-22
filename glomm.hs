{-# language ScopedTypeVariables #-}

import Control.Arrow ((>>>))
import Data.List
import Data.Monoid
import Data.Maybe
import Control.Monad (when)
import Safe
import Development.Shake
import Development.Shake.FilePath
import System.Environment
import System.Process
import Control.Applicative ((<$>))
import Data.Either
import Data.List
import System.FilePath (takeDirectory)
import System.Directory (
    createDirectoryIfMissing,
    getCurrentDirectory,
    canonicalizePath,
    renameFile,
    removeFile)

import Translate hiding (imports)


main = shakeArgs shakeOptions{
                     shakeFiles = "_make/",
                     shakeProgress = progressSimple,
                     shakeVerbosity = Normal
  } $ do
    phony "clean" $ do
        removeFilesAfter "_make" ["//*"]

    -- should be *.hs.hi
    ["_make//*.hcr", "_make//*.hi"] *>> \ [coreFile, hiFile] -> do
        hsFile <- searchHaskellFile False $ dropDirectory1 $ dropExtension coreFile
        direct <- directImports ("_make" </> hsFile)
        let importHiFiles = map (\ f -> "_make" </> replaceBootExtension f ".hi") direct
        need (hsFile : importHiFiles)
        ghcOptions <- readGhcOptions
        system' "ghc" $
            hsFile :
            "-fext-core" :
            "-S" :
            "-outputdir" : "_make" :
            "-i_make" :
            "-hide-all-packages" :
            "-package" : "base" :
            ghcOptions ++
            []
        liftIO $ do
            removeFile (replaceExtension hsFile ".s")
            renameFile (replaceExtension hsFile ".hcr") coreFile

    "_make//*.hi-boot" *> \ hiBootFile -> do
        hsFile <- searchHaskellFile True $ dropDirectory1 $ dropExtension hiBootFile
        direct <- directImports ("_make" </> hsFile)
        let importHiFiles = map (\ f -> "_make" </> replaceBootExtension f ".hi") direct
        need (hsFile : importHiFiles)
        ghcOptions <- readGhcOptions
        system' "ghc" $
            hsFile :
            "-c" :
            "-outputdir" : "_make" :
            "-i_make" :
            "-hide-all-packages" :
--             "-package" : "base" :
--             "-ohi" : hiBootFile :
            ghcOptions ++
            []

    "_make//*.hcr.js" *> \ jsFile -> do
        hsFile <- searchHaskellFile False $
            dropDirectory1 $ dropExtension $ dropExtension jsFile
        need ["pre.js", "ghcPrim.js", "post.js"]
        neededPackages <-
            map (\ f -> "packageDB" </> f <.> "package.js.opt") <$>
            readFileLines "packages"
        need neededPackages
        transitive <- transitiveImports ("_make" </> hsFile)
        let coreFile = dropExtension jsFile
            importCoreFiles =
                map (\ f -> "_make" </> replaceExtension f ".hcr") transitive
        need (coreFile : importCoreFiles)
        liftIO $ putStrLn ("compiling " ++ show (coreFile : importCoreFiles))
        liftIO $ compileFiles (MainModule coreFile importCoreFiles neededPackages) jsFile

    "_make//*.transitiveImports" *> \ importsFile -> do
        direct :: [FilePath] <-
            directImports (dropExtension importsFile)
        transitive :: [FilePath] <-
            concat <$>
            mapM transitiveImports
            (map ("_make" </>) direct)
        liftIO $ writeFile importsFile $ unlines $ nub (direct ++ transitive)

    -- use ghc -M to generate file of direct dependencies
    "_make//*.directImports" *> \ importsFile -> do
        let hsFile = dropDirectory1 $ dropExtension importsFile
            ghcMakeFile = importsFile <.> "ghcMakeFile"
            oFile = replaceExtension hsFile ".o"
        need [hsFile]
        ghcOptions :: [String] <- readGhcOptions
        packages <- readFileLines "packages"
        system' "ghc" $
            "-M" : "-dep-makefile" : ghcMakeFile :
            "-hide-all-packages" :
            (concatMap (\ p -> "-package" : p : []) packages) ++
            ghcOptions ++
            hsFile :
            []
        directImports <- parseImports oFile hsFile =<< readFile' ghcMakeFile
        liftIO $ writeFile importsFile $ unlines directImports

    -- use google's closure compiler to optimize javascript
    "_make//*.js.opt" *> \ optFile -> do
        let jsFile = dropExtension optFile
        need [jsFile]
        -- google's closure compiler
        system' "closure" $
            "--js" : jsFile :
            "--js_output_file" : optFile :
            []

    "_make/*.package.js" *> \ jsFile -> do
        -- build a package
        let package = dropDirectory1 $ dropExtension $ dropExtension jsFile
        coreFiles <-
            map (\ f -> "_make" </> f <.> "hcr") <$>
            filter (not . ("#" `isPrefixOf`)) <$>
            readFileLines (package <.> "modules")
        need coreFiles
        liftIO $ putStrLn (package ++ ": " ++ show coreFiles)
        liftIO $ compileFiles (Package package coreFiles) jsFile

    "packageDB//*.package.js.opt" *> \ packageFile -> do
        -- calls glomm to build a package and moves it to packageDB
        need ["glomm"]
        glommBinary <- liftIO $ canonicalizePath "glomm"
        let package = dropDirectory1 $ dropExtension $ dropExtension $ dropExtension packageFile
            ghcOptionsFile = package <.> "ghcOptions"
            modulesFile = package <.> "modules"
            packageDir = "ghc-7.6.3/libraries/" </> package
        need $ 
            packageDir :
            packageDir </> modulesFile :
            packageDir </> "ghcOptions" :
            packageDir </> "packages" :
            []
        let localTarget = "_make" </> package <.> "package.js.opt"
        systemCwd packageDir glommBinary $
            "--jobs=4" :
            localTarget :
            []
        copyFile' (packageDir </> localTarget) ("packageDB" </> package <.> "package.js.opt")

    "ghc-7.6.3/libraries//*.modules" *> \ target -> do
        let modulesFile = takeFileName target
        copyFile' modulesFile target

    "ghc-7.6.3/libraries/*/ghcOptions" *> \ target -> do
        let package = head $ splitDirectories $ dropDirectory1 $ dropDirectory1 $ target
        copyFile' (package <.> "ghcOptions") target

    "ghc-7.6.3/libraries/*/packages" *> \ target -> do
        liftIO $ writeFile target ""

    return ()

-- | Searches for a file with either .lhs or .hs
searchHaskellFile :: Bool -> String -> Action FilePath
searchHaskellFile bootFile file = do
    let addBoot = if bootFile then (++ "-boot") else id
        lhsFile = addBoot (file <.> ".lhs")
        hsFile  = addBoot (file <.> ".hs")
    lhsExists <- doesFileExist lhsFile
    hsExists <- doesFileExist hsFile
    return $ if lhsExists
        then lhsFile
        else if hsExists
        then hsFile
        else error ("can't find " ++ hsFile ++ " or " ++ lhsFile)

readGhcOptions :: Action [String]
readGhcOptions =
    readNote ("unable to read ghcOptions file") <$>
    unwords <$>
    filter (\ l -> not ("#" `isPrefixOf` l)) <$> 
    lines <$>
    readFile' "ghcOptions"

directImports :: FilePath -> Action [FilePath]
directImports file =
    readFileLines (file <.> "directImports")

transitiveImports :: FilePath -> Action [FilePath]
transitiveImports file =
    readFileLines (file <.> "transitiveImports")

parseImports :: FilePath -> FilePath -> String -> Action [FilePath]
parseImports oFile hsFile =
    lines >>>
    filter ((oFile ++ " ") `isPrefixOf`) >>>
    map (words >>> last) >>>
    map sort >>>
    catMaybes >>>
    map (either (searchHaskellFile False) (searchHaskellFile True)) >>>
    sequence
  where
    sort :: FilePath -> Maybe (Either FilePath FilePath)
    sort f | f == hsFile = Nothing
    sort f = Just $ case takeExtension f of
        ".hi-boot" -> Right $ dropExtension f
        ".hi"      -> Left $ dropExtension f
        _ -> error $ show ("parseImports", hsFile, f)

replaceBootExtension :: FilePath -> String -> FilePath
replaceBootExtension file extension =
    if "-boot" `isSuffixOf` file
       then replaceExtension file (extension ++ "-boot")
       else replaceExtension file extension
