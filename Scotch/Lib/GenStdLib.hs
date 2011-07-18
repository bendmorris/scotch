module Scotch.Lib.GenStdLib where

import System
import System.Environment.Executable
import System.Directory
import System.IO
import Control.Concurrent
import Data.List
import System.Console.Haskeline
import System.Console.Haskeline.IO
import System.Console.Haskeline.Completion
import Scotch.Config
import Scotch.Parse.Parse as Parse
import Scotch.Eval.ReadFile
import Scotch.Types.Types
import Scotch.Types.Exceptions
import Scotch.Types.Bindings
import Scotch.Types.Hash
import Scotch.Types.Interpreter
import Scotch.Eval.Eval
import Scotch.Eval.Substitute


main = do exePath <- getExecutablePath
          exeMod <- getModificationTime (exePath)
          importStdLib <- importFile (InterpreterSettings {verbose = False, interpret = False, strict = False, exePath = exePath, exeMod = exeMod, stdLib = emptyHash}) 
                                     False
                                     ["std", "lib"] ["std", "lib"]
          case importStdLib of
            (True, b) -> do writeFile "Scotch/Lib/StdLib.hs"
                                      ("module Scotch.Lib.StdLib where\n\nimport Scotch.Types.Types\n\n"
                                       ++ "stdlib = " ++ show b)

            otherwise -> do putStrLn "Failed to import std.lib"
