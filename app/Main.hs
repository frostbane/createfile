{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main
    ( main
    ) where

import Data.Text hiding (length)
import qualified Data.Text as T
import Fb.Arguments
import Fb.Exit

main :: IO ()
main = do
    args <- getArguments
    checkArgs args

checkArgs :: [Text] ->  IO ()
checkArgs args = do
    prog <- (return . T.unpack) =<< getProgramName
    return ()
    if | count < 2 -> exitWith ((-1) :: Int) << putStrLn $ prog ++ " requires 2 arguments."
       | otherwise -> return ()
  where
    count = length args

infixr 0 <<
(<<) :: (Monad m) => m a -> m b -> m a
(<<) = flip (>>)
