{-# OPTIONS -Wall -Wno-unused-matches #-}

module Fb.Arguments
    ( getWorkingDir,
      getProgramName,
      getArguments,
    )
  where

import System.Environment
    ( getProgName,
      getArgs,
    )
import System.Directory (getCurrentDirectory)
import System.IO
    ( hSetEncoding,
      stdin,
      stdout,
      utf8,
    )
import qualified Data.Text as T
import Data.Text
import Data.Text.IO (hPutStrLn)


getWorkingDir :: IO (Text)
getWorkingDir = do
    cwd <- getCurrentDirectory
    return $ T.pack cwd

getProgramName :: IO (Text)
getProgramName = do
    name <- getProgName
    return $ T.pack name

getArguments :: IO ([Text])
getArguments = do
    hSetEncoding stdin utf8
    hSetEncoding stdout utf8
    args <- getArgs
    return $ Prelude.map T.pack args

