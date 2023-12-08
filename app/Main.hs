{-# LANGUAGE MultiWayIf
           , OverloadedStrings
#-}
{-# OPTIONS_GHC
      -Wno-name-shadowing
      -Wno-dodgy-imports
#-}

module Main
    ( main
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.IO hiding (createFile)
import Control.Monad

import Fb.Arguments
import Fb.Exit
import Lib

main :: IO ()
main = do
    args <- getArguments
    checkArgs args
    let contents = (createContents . tail) args
    createFile (head args) contents

checkArgs :: [Text] ->  IO ()
checkArgs args = do
    let enoughArgs = argumentCountIsEnough args

    when (not enoughArgs) $ exit (-1) "[ERROR] Two or more arguments are required."

    let filename = head args
    afile <- checkFile filename
    existingPath <- pathExists filename
    hasPerms <- checkPerms filename

    if | not enoughArgs   -> exit (-1) "[ERROR] Two or more arguments are required."
       | not existingPath -> exit (-2) "[ERROR] Output path does not exist."
       | not afile        -> exit (-3) "[ERROR] Output file is a directory."
       | not hasPerms     -> exit (-4) "[ERROR] Unable to read/write output file."
       | otherwise        -> return ()

  where

checkPerms :: Text -> IO (Bool)
checkPerms filename = do
    existingFile <- fileExists filename
    if existingFile then do
        hasPerms <- hasPermission filename
        return hasPerms
    else
        return True

checkFile :: Text -> IO Bool
checkFile = isFile

exit :: Int -> String -> IO ()
exit code msg = do
    prog <- (return . T.unpack) =<< getProgramName
    exitWith code << putStrLn $ prog ++ " " ++ msg

createContents :: [Text] -> Text
createContents = T.intercalate "\n"

createFile :: Text -> Text -> IO ()
createFile filename contents = do
    writeFile (T.unpack filename) (T.unpack contents)

infixr 0 <<
(<<) :: (Monad m) => m a -> m b -> m a
(<<) = flip (>>)
