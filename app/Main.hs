{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC
      -Wno-name-shadowing
      -Wno-unused-imports
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
    pathExists <- (checkPathExists . head) args
    return ()
    if | not $ checkArgumentCount args -> exit (-1) "[ERROR]. 2 or more arguments are required."
       | not pathExists -> exit (-2) "[ERROR] Output path does not exist."
       | otherwise -> return ()

exit :: Int -> String -> IO ()
exit code msg = do
    prog <- (return . T.unpack) =<< getProgramName
    exitWith code << putStrLn $ prog ++ " " ++ msg


infixr 0 <<
(<<) :: (Monad m) => m a -> m b -> m a
(<<) = flip (>>)
