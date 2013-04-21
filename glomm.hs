

import Development.Shake
import Development.Shake.FilePath
import System.Environment

import Translate


main = shakeArgs shakeOptions{shakeFiles = "_make/", shakeProgress = progressSimple} $ do
     phony "clean" $ removeFilesAfter "_make" ["//*"]

     "_make//*.hs.hcr" *> \ coreFile -> do
        let hsFile = dropDirectory1 $ dropExtension coreFile
        need [hsFile]
        system' "ghc" $
            hsFile :
            "-fext-core" :
            "-S" :
            "-outputdir" : "_make" :
            []
        system' "mv" $
            (replaceExtension hsFile ".hcr") :
            coreFile :
            []

     "_make//*.hs.hcr.js" *> \ jsFile -> do
        let coreFile = dropExtension jsFile
        need [coreFile]
        liftIO $ compileFiles [coreFile] jsFile

     return ()
