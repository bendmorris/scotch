module Scotch.Config where

import System.Environment.Executable
import System.Info


major    = "0"
minor    = "5"
revision = "0"

version = major ++ "." ++ minor ++ "." ++ revision

environment = os

libraryPath = do fullPath <- splitExecutablePath
                 return $ (fst fullPath) ++ "scotch.lib"
