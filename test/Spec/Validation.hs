module Spec.Validation
    ( validationSpec
    ) where

import Test.Hspec

validationSpec :: Spec
validationSpec =
  describe "program argument validation" $ do
    context "argument count" $ do
      it "should have 2 or more arguments" $ do
        pendingWith "not yet implemeted"
    context "target output file" $ do
      it "output folder should exist" $ do
        pendingWith "not yet implemeted"
      it "missing file (create output file) should be allowed" $ do
        pendingWith "not yet implemeted"
      it "existing output file should be readable and writable" $ do
        pendingWith "not yet implemeted"

