module Scotch.Config where

import System.Environment.Executable
import System.Info


version = "0.5.0"

environment = os

libraryPath = do fullPath <- splitExecutablePath
                 return $ (fst fullPath) ++ "scotch.lib"
