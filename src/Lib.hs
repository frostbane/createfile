module Lib
    ( checkArgumentCount
    , checkPathExists
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Directory

checkArgumentCount :: [Text] -> Bool
checkArgumentCount args = count >= 2
  where
    count = length args

checkPathExists :: Text -> IO Bool
checkPathExists = doesPathExist . T.unpack

