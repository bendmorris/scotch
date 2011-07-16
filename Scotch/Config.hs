module Scotch.Config where

import System.Environment.Executable
import System.Info


major    = "0"
minor    = "5"
revision = "0"

version = major ++ "." ++ minor ++ "." ++ revision

environment = os


libraryPath :: IO String
libraryPath = do return $ "/usr/lib/scotch" ++ major ++ "." ++ minor
