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

module Scotch.Types.Hash where

import Data.Char
import Scotch.Types.Types

hashSize = 100

type HashFunction a = (a -> Int)

hashKey [] = 0
hashKey (h:t) = (ord h) + hashKey t

strHash :: HashFunction String
strHash s = mod (hashKey s) hashSize

emptyHash = [[] | n <- [1..hashSize]]

bucketMember :: (Eq a) => (HashFunction a) -> a -> [(a, b)] -> Maybe b
bucketMember _ s [] = Nothing
bucketMember f s (h:t) = if fst h == s then Just (snd h) else bucketMember f s t

hashMember :: (Eq a) => (HashFunction a) -> a -> [[(a, b)]] -> Maybe b
hashMember f s h = bucketMember f s (h !! (f s))

removeFromBucket :: (Eq a) => (a, b) -> [(a, b)] -> [(a, b)]
removeFromBucket a [] = []
removeFromBucket a (h:t) = if (fst h) == (fst a)
                           then removeFromBucket a t
                           else h : removeFromBucket a t

makeHash :: (Eq a) => (HashFunction a) -> [(a, b)] -> [[(a, b)]] -> [[(a, b)]]
makeHash _ [] r = r
makeHash f (h:t) r = 
  makeHash f t 
  [(if f (fst h) == i
    then [h]
    else [])
    ++ (removeFromBucket h (r !! i))
   | i <- [0..(hashSize-1)]]
