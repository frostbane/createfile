module Lib
    ( argumentCountIsEnough
    , pathExists
    , fileExists
    , hasPermission
    , isFile
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.FilePath

argumentCountIsEnough :: [Text] -> Bool
argumentCountIsEnough args = count >= 2
  where
    count = length args

pathExists :: Text -> IO Bool
pathExists filename = do
    doesPathExist dir
  where
    (dir, _) = (splitFileName . T.unpack) filename

fileExists :: Text -> IO Bool
fileExists = doesFileExist . T.unpack

hasPermission :: Text -> IO Bool
hasPermission filename = do
    perms <- (getPermissions . T.unpack) filename
    -- (putStrLn .show) filename
    -- (putStrLn .show) perms
    let hasPerms = readable perms && writable perms
    return hasPerms

isFile :: Text -> IO Bool
isFile filename = do
    isDir <- (doesPathExist . T.unpack) filename
    return $ not isDir

