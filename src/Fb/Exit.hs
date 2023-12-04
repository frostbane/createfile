{-# OPTIONS -Wall -Wno-unused-matches -Wno-unused-do-bind #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Fb.Exit
    ( exitWith
    )
  where

import qualified System.Exit (exitWith)
--import qualified GHC.IO.Exception (ExitCode)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))


class ExitWithCode a where
    exitWith :: a -> IO ()

instance ExitWithCode Int where
    exitWith :: Int -> IO ()
    exitWith 0 = System.Exit.exitWith ExitSuccess
    exitWith n = System.Exit.exitWith $ ExitFailure n

instance ExitWithCode System.Exit.ExitCode where
    exitWith :: System.Exit.ExitCode -> IO ()
    exitWith c = System.Exit.exitWith c

instance ExitWithCode Bool where
    exitWith :: Bool -> IO ()
    exitWith True = System.Exit.exitWith ExitSuccess
    exitWith _    = System.Exit.exitWith $ ExitFailure 1


-- exit :: IO ()
-- exit = exitWith (0 :: Int)
