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

module Scotch.Types.Exceptions where

import Scotch.Types.Types

exTakeNonList = Exception $ "Take from non-list"
exTakeNonInt = Exception $ "Non-integer in take expression"
exCantConvert a b = Exception $ "Can't convert " ++ a ++ " to " ++ b ++ "."
exNoMatch f args = Exception $ "Function " ++ show f ++ " " ++ show args ++ " doesn't match any defined patterns"
exNonNumSubs n = Exception $ "Non-numerical subscript " ++ show n
exNotInList n = Exception $ "Member " ++ show n ++ " not in list"
exNotInHash n = Exception $ "Key " ++ show n ++ " not in hash"
exNotList a = Exception $ show a ++ " is not a list"
exTypeMismatch a b f = Exception $ "Type mismatch: " ++ show a ++ " " ++ f ++ " " ++ show b
exNotBool a = Exception $ show a ++ " is not a boolean value"
exNonIntInRange = Exception "Non-integer argument in range"
exNonStrFilename = Exception "Non-string filename"
exWriteNonString a = Exception $ "Can't write non-string " ++ show a ++ " to file"
exWriteNonFile a = Exception $ "Can't write to non-file " ++ show a
exNoCaseMatch a = Exception $ "Case expression " ++ show a ++ " doesn't match any defined patterns"
exFileDNE = Exception "File does not exist"
exInvalidFile = Exception "Invalid file"
exUnableToEval a = Exception $ "Unable to evaluate: " ++ show a
exImproperCall v = Exception $ show v ++ " is not a function"
exWrongNumArgs = Exception $ "Wrong number of arguments for lambda call"
exEvalMultiple = Exception "Can't interpret multiple expressions simultaneously"
exNonTerminatingFunction f args = Exception $ "Non-terminating function: " ++ show f ++ " " ++ show args
