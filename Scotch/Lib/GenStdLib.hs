module Scotch.Lib.GenStdLib where

import System
import System.Environment.Executable
import System.Directory
import System.IO
import Control.Concurrent
import Data.List
import Data.List.Utils
import System.Console.Haskeline
import System.Console.Haskeline.IO
import System.Console.Haskeline.Completion
import Scotch.Config
import Scotch.Eval.ReadFile
import Scotch.Types.Types
import Scotch.Types.Hash
import Scotch.Types.Interpreter

          
modules = [fst m | m <- stdModules]

main = do exePath <- getExecutablePath
          exeMod <- getModificationTime (exePath)
          importStdLib <- importFile (InterpreterSettings {verbose = True, interpret = False, strict = False, exePath = exePath, exeMod = exeMod, stdLib = emptyHash}) 
                                     False
                                     ["std", "lib"] ["std", "lib"]
          stdlib <- case importStdLib of
            (True, b) -> do return b
            otherwise -> do putStrLn "Failed to import std.lib"
                            return emptyHash
          imports <- iolist [iopair
                             (importFile (InterpreterSettings {verbose = True, interpret = False, strict = False, exePath = exePath, exeMod = exeMod, stdLib = stdlib})
                                         False
                                         m m, m)
                             | m <- modules
                             ]
          writeFile "Scotch/Lib/StdLib.hs"
                    ("module Scotch.Lib.StdLib where\n\nimport Scotch.Types.Types\n\n"
                     ++ join "\n" [case fst m of
                                     (True, b) -> (join "" (snd m)) ++ " = " ++ show b
                                     otherwise -> ""
                                   | m <- (importStdLib, ["std", "lib"]) : imports])
