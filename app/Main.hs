{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC
      -Wno-name-shadowing
#-}

module Main
    ( main
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.IO

import Fb.Arguments
import Fb.Exit
import Lib

main :: IO ()
main = do
    args <- getArguments
    checkArgs args

checkArgs :: [Text] ->  IO ()
checkArgs args = do
    let filename = head args
        enoughArgs = argumentCountIsEnough args
    existingPath <- pathExists filename
    existingFile <- fileExists filename
    hasPerms <- hasPermission filename
    return ()
    if | not enoughArgs                 -> exit (-1) "[ERROR] Two or more arguments are required."
       | not existingPath               -> exit (-2) "[ERROR] Output path does not exist."
       | not (existingFile && hasPerms) -> exit (-3) "[ERROR] Unable to read/write output file."
       | otherwise -> return ()

exit :: Int -> String -> IO ()
exit code msg = do
    prog <- (return . T.unpack) =<< getProgramName
    exitWith code << putStrLn $ prog ++ " " ++ msg


infixr 0 <<
(<<) :: (Monad m) => m a -> m b -> m a
(<<) = flip (>>)
