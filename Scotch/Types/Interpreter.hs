{-  This file is part of Scotch.

    Scotch is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Scotch is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Scotch.  If not, see <http://www.gnu.org/licenses/>.
-}

module Scotch.Types.Interpreter where

import System.Time
import Scotch.Types.Types
import Scotch.Types.Bindings

data InterpreterSettings = InterpreterSettings {
    verbose :: Bool,
    strict :: Bool,
    interpret :: Bool,
    exePath :: FilePath,
    exeMod :: System.Time.ClockTime,
    stdLib :: VarDict
}
