import Test.Hspec

import Spec.Validation as Run

main :: IO ()
main = do
    putStrLn "CreateFile Test"
    hspec $ do
        Run.validationSpec

