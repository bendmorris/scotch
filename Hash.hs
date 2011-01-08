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

module Hash where

import Data.Char
import Types

hashSize = 500

hashKey [] = 0
hashKey (h:t) = (ord h) + hashKey t

hashLoc s = mod (hashKey s) hashSize

emptyHash = [[] | n <- [1..hashSize]]

bucketMember :: String -> [(String, Value)] -> Value
bucketMember s [] = Undefined $ "Member " ++ s ++ " not found in hash"
bucketMember s (h:t) = if fst h == s then snd h else bucketMember s t

hashMember :: String -> [[(String, Value)]] -> Value
hashMember s h = bucketMember s (h !! (hashLoc s))

makeHash' :: [(String, Value)] -> [[(String, Value)]] -> [[(String, Value)]]
makeHash' [] r = r
makeHash' (h:t) r = makeHash' t 
                    [(if hashLoc (fst h) == i
                      then [h]
                      else [])
                     ++ (r !! i)
                     | i <- [0..(hashSize-1)]]
makeHash :: [(String, Value)] -> [[(String, Value)]]
makeHash s = makeHash' s emptyHash
