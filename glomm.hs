{-# language ScopedTypeVariables #-}

import Control.Arrow ((>>>))
import Data.List
import Data.Monoid
import Data.Maybe
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
    canonicalizePath)

import Translate hiding (imports)


main = shakeArgs shakeOptions{
                     shakeFiles = "_make/",
                     shakeProgress = progressSimple,
                     shakeVerbosity = Loud
  } $ do
    phony "clean" $ do
        removeFilesAfter "_make" ["//*"]

    -- should be *.hs.hi
    ["_make//*.hcr", "_make//*.hi"] *>> \ [coreFile, hiFile] -> do
        let moduleFile = dropExtension coreFile
        hsFile <- searchHaskellFile (dropDirectory1 moduleFile)
        deps :: HaskellDependencies <- readDependencies
             (moduleFile <.> "directImports")
        let importHiFiles = map (\ f -> "_make" </> f <.> "hi") $ imports deps
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
        system' "rm" [replaceExtension hsFile ".s"]
        system' "mv" $
            (replaceExtension hsFile ".hcr") :
            coreFile :
            []

    "_make//*.hi-boot" *> \ hiBootFile -> do
        error $ show ("hi-boot", hiBootFile)

    "_make//*.hcr.js" *> \ jsFile -> do
        need ["pre.js", "ghcPrim.js", "post.js"]
        neededPackages <- 
            map (\ f -> "packageDB" </> f <.> "package.js.opt") <$>
            readFileLines "packages"
        need neededPackages
        let coreFile = dropExtension jsFile
            moduleFile = dropExtension $ dropExtension coreFile
        transitiveDeps <- readDependencies (moduleFile <.> "transitiveImports")
        let importCoreFiles =
                map (\ f -> "_make" </> f <.> ".hcr")
                    (imports transitiveDeps)
        need (coreFile : importCoreFiles)
        liftIO $ putStrLn ("compiling " ++ show (coreFile : importCoreFiles))
        liftIO $ compileFiles (MainModule coreFile importCoreFiles neededPackages) jsFile

    "_make//*.transitiveImports" *> \ importsFile -> do
        directDeps :: HaskellDependencies <-
            readDependencies (replaceExtension importsFile ".directImports")
        transitiveDeps :: [HaskellDependencies] <-
            mapM readDependencies
                 (map (\ f -> ("_make" </> f <.> "transitiveImports"))
                      (imports directDeps))
        liftIO $ writeFile importsFile $ show (mconcat (directDeps : transitiveDeps))

    -- use ghc -M to generate file of direct dependencies
    "_make//*.directImports" *> \ importsFile -> do
        hsFile <- searchHaskellFile $ dropDirectory1 $ dropExtension importsFile
        let ghcMakeFile = importsFile <.> "ghcMakeFile"
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
        directImports <- parseImports oFile hsFile <$> readFile' ghcMakeFile
        liftIO $ writeFile importsFile $ show directImports

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
        modules <-
            map (\ f -> "_make" </> f <.> "hcr") <$>
            filter (not . ("#" `isPrefixOf`)) <$>
            readFileLines (package <.> "modules")
        need modules
        liftIO $ putStrLn (package ++ ": " ++ show modules)
        liftIO $ compileFiles (Package package modules) jsFile

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
searchHaskellFile :: String -> Action FilePath
searchHaskellFile file = do
    let lhsFile = file <.> ".lhs"
        hsFile  = file <.> ".hs"
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


data HaskellDependencies = HaskellDependencies {
    imports :: [String],
    bootImports :: [String]
  }
    deriving (Show, Read)

instance Monoid HaskellDependencies where
    mappend a b =
        HaskellDependencies
            (nub (      imports a ++       imports b))
            (nub (  bootImports a ++   bootImports b))
    mempty = HaskellDependencies [] []

readDependencies :: FilePath -> Action HaskellDependencies
readDependencies file =
    readNote "cannot read dependencies" <$>
    readFile' file

parseImports :: FilePath -> FilePath -> String -> HaskellDependencies
parseImports oFile hsFile makeFile =
    HaskellDependencies directImports bootImports
  where
    directImports = lefts parsed
    bootImports = rights parsed
    parsed = parse makeFile
    parse =
        lines >>>
        filter ((oFile ++ " ") `isPrefixOf`) >>>
        map (words >>> last) >>>
        map sort >>>
        catMaybes
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
