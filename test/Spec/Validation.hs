{-# LANGUAGE OverloadedStrings #-}

module Spec.Validation
    ( validationSpec
    ) where

import Test.Hspec
import qualified Data.Text as T
import System.IO
import System.Directory
import System.FilePath.Posix
import Control.Monad.IO.Class
import Control.Monad.Trans.Identity

import qualified Lib as L

validationSpec :: Spec
validationSpec = do
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

      it "missing file (create output file) should be allowed" $ do
        tmpDir <- getTemporaryDirectory
        let yyy = tmpDir </> "yyy.txt"

        do
            e <- doesFileExist yyy
            if e then
               removeFile yyy
            else
                return ()

        yyyExists <- doesFileExist yyy
        yyyExists `shouldBe` False

        yyyFileExists <- (L.fileExists . T.pack) yyy
        yyyFileExists `shouldBe` False

        writeFile yyy "some content"

        newYYYFileExists <- (L.fileExists . T.pack) yyy
        newYYYFileExists `shouldBe` True

        removeFile yyy

      it "existing output file should be readable and writable" $ do
        pendingWith "not yet implemeted"

