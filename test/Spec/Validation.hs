{-# LANGUAGE OverloadedStrings #-}

module Spec.Validation
    ( validationSpec
    ) where

import Test.Hspec
import System.IO
import System.Directory
import Control.Monad.IO.Class
import Control.Monad.Trans.Identity

import Lib

validationSpec :: Spec
validationSpec = do
  it "info" $ do
    pwd <- (return . show) =<< getCurrentDirectory
    putStrLn $ "\t" ++ pwd
  describe "program argument validation" $ do
    context "argument count" $ do
      it "should have 2 or more arguments" $ do
        argumentCountIsEnough ["a"] `shouldBe` False
        argumentCountIsEnough ["a", "b"] `shouldBe` True
        argumentCountIsEnough ["a", "b", "c"] `shouldBe` True
    context "target output file" $ do
      it "output folder should exist" $ do
        pendingWith "not yet implemeted"
      it "missing file (create output file) should be allowed" $ do
        pendingWith "not yet implemeted"
      it "existing output file should be readable and writable" $ do
        pendingWith "not yet implemeted"

