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
import System.Directory
import System.FilePath.Posix
import Control.Monad.IO.Class
import Control.Exception (bracket)
import Control.Monad.Trans.Identity

import qualified Lib as L

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

      it "existing output file should be readable and writable" $ do
        pendingWith "not yet implemeted"

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
          let yyy = tmpDir </> "yyy.txt"
          (\x -> x `shouldBe` False) =<< doesFileExist yyy
          (\x -> x `shouldBe` False) =<< (L.fileExists . T.pack) yyy

          writeFile yyy "some content"

          (\x -> x `shouldBe` True) =<< (L.fileExists . T.pack) yyy
          (L.fileExists . T.pack) yyy `ioShouldBe` False


ioShouldBe :: (Show a, Eq a) => IO (a) -> a -> Expectation
ioShouldBe a q = do
    ioresult <- a
    ioresult `shouldBe` q

-- iobe :: (Show a, Eq a) => IO (a) -> a -> IO Property
-- iobe a q = do
--     ioresult <- a
--     if | ioresult == q -> return $ property True
--        | otherwise     -> return $ property False

withNoTempYYY action =
    bracket
        removeTempYYY
        action
        (\_ -> removeTempYYY)

-- removeTempYYY :: ActionWith () -> IO ()
removeTempYYY = do
  tmpDir <- getTemporaryDirectory
  let yyy = tmpDir </> "yyy.txt"
  r <- randomIO :: IO Int
  putStrLn $ "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx " <> (show r)

  do
    e <- doesFileExist yyy
    if e then
       removeFile yyy
    else
        return ()
