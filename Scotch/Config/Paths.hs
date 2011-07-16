module Scotch.Config.Paths where

import System.Environment.Executable


libraryPath = do fullPath <- splitExecutablePath
                 return $ (fst fullPath) ++ "scotch.lib"
