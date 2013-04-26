#!/usr/bin/env runhaskell


import Control.Applicative
import Control.Monad (when)
import Data.Foldable
import Data.List
import System.Directory
import System.Exit
import System.FilePath
import System.Process


main :: IO ()
main = do
    files <-
        reverse <$>
        sort <$>
        map ("Test" </>) <$>
        filter ((== ".hs") . takeExtension) <$>
        getDirectoryContents "Test"
    forM_ files testFile

testFile :: FilePath -> IO ()
testFile file = do
    putStrLn ("testing " ++ file ++ "...")
    let jsFile = ("_make" </> file <.> ".hcr.js")
    sysNoOut "runghc" ["-O1", "glomm.hs", jsFile, "-p", "-V"]
    output <- sys "nodejs" [jsFile]
    expected <- readFile (file <.> "exp")
    when (expected /= output) $
        error (file ++ "\n" ++ show expected ++ "\n    /=\n" ++ show output)

sys :: String -> [String] -> IO String
sys cmd options = do
    (ec, output, err) <- readProcessWithExitCode cmd options ""
    putStrLn output
    putStrLn err
    when (ec /= ExitSuccess) $ do
        error ("fail: " ++ show ec)
    return output

sysNoOut :: String -> [String] -> IO ()
sysNoOut cmd options = do
    ec <- system (unwords (cmd : options))
    when (ec /= ExitSuccess) $ do
        error ("fail: " ++ show ec)
