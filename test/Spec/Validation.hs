{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Spec.Validation
    ( validationSpec
    ) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Text as T
import System.IO
import System.Random
import System.Posix.Files
import System.Directory
-- import System.Directory.Internal
import System.FilePath.Posix
import Control.Monad.IO.Class
import Control.Exception (bracket)
import Control.Monad.Trans.Identity

import qualified Lib as L

tempfilename = "@create-file-yyy.txt"

validationSpec :: Spec
validationSpec = do
  testArguments
  testFileExist

testArguments = do
  it "info" $ do
    pwd <- (return . show) =<< getCurrentDirectory
    tmpDir <- (return . show) =<< getTemporaryDirectory
    putStrLn $ "\tworking directory: " ++ pwd
    putStrLn $ "\ttemporary directory: " ++ tmpDir

  describe "program argument validation" $ do
    context "argument count" $ do
      it "should have 2 or more arguments" $ do
        L.argumentCountIsEnough ["a"] `shouldBe` False
        L.argumentCountIsEnough ["a", "b"] `shouldBe` True
        L.argumentCountIsEnough ["a", "b", "c"] `shouldBe` True

testFileExist = do
  describe "program argument validation" $ do
    context "target output file" $ do
      it "output folder should exist" $ do
        homeDir <- getHomeDirectory
        homeExists <- doesPathExist homeDir
        homeExists `shouldBe` True

        pathExists <- L.pathExists (T.pack homeDir)
        pathExists `shouldBe` True

        let xxx = homeDir </> "xxxx" </> "file.txt"
        xxxExists <- doesPathExist xxx
        xxxExists `shouldBe` False

        xxxPathExists <- (L.pathExists . T.pack) xxx
        xxxPathExists `shouldBe` False

      around withNoTempYYY $ do
        it "missing file (create output file) should be allowed" $ do
          tmpDir <- getTemporaryDirectory
          let yyy = tmpDir </> tempfilename

          doesFileExist yyy `shouldReturn` False
          (L.fileExists . T.pack) yyy `shouldReturn` False

          writeFile yyy "some content"

          (L.fileExists . T.pack) yyy >>= (`shouldBe` True)
          (L.fileExists . T.pack) yyy `shouldReturn` True
          nop

      around withNoTempYYY $ do
        xit "existing output file should be readable and writable" $ do
          tmpDir <- getTemporaryDirectory
          let yyy = tmpDir </> tempfilename

          writeFile yyy "some content"
          setFileMode yyy 0o400

          (L.fileExists . T.pack) yyy >>= (`shouldBe` True)
          (L.hasPermission . T.pack) yyy `shouldReturn` False
          nop

nop :: Expectation
nop = True `shouldBe` True

withNoTempYYY action =
    bracket
        removeTemp
        action
        (\_ -> removeTemp)
  where
    removeTemp :: IO ()
    removeTemp = do
        tmpDir <- getTemporaryDirectory
        let yyy = tmpDir </> tempfilename
        -- r <- randomIO :: IO Int
        -- putStrLn $ "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx " <> (show r)

        do
            e <- doesFileExist yyy
            if e then
                removeFile yyy
            else
                return ()
