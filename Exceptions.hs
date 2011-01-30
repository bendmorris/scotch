module Exceptions where

import Types

exTakeNonList = Exception $ "Take from non-list"
exTakeNonInt = Exception $ "Non-integer in take expression"
exCantConvert a b = Exception $ "Can't convert " ++ a ++ " to " ++ b ++ "."
exNoMatch f args = Exception $ "Function " ++ show f ++ " " ++ show args ++ " doesn't match any defined patterns"
exNonNumSubs n = Exception $ "Non-numerical subscript " ++ show n
exNotInList n = Exception $ "Member " ++ show n ++ " not in list"
exNotList a = Exception $ show a ++ " is not a list"
exTypeMismatch a b f = Exception $ "Type mismatch: " ++ show a ++ " " ++ show f ++ " " ++ show b
exNotBool a = Exception $ show a ++ " is not a boolean"
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
