module Scotch.Types.Binary where

import Data.Binary
import Scotch.Types.Types


instance Binary(Value) where
    put (Str s) =           do put (4 :: Word8)
                               put s
    put (NumInt n) =        do put (5 :: Word8)
                               put n
    put (NumFloat n) =      do put (6 :: Word8)
                               put n
    put (Bit b) =           do put (7 :: Word8)
                               put b
    put (Hash h) =          do put (9 :: Word8)
                               put h
    put (Lambda i e) =      do put (11 :: Word8)
                               put i
                               put e
    put (Proc p) =          do put (12 :: Word8)
                               put p
    put (Thread th) =       do put (13 :: Word8)
                               put th
    put (Null) =            do put (14 :: Word8)
    put (Undefined s) =     do put (15 :: Word8)
                               put s
    put (File f) =          do put (16 :: Word8)
                               put f
    get = do t <- get :: Get Word8
             case t of 
               4 ->     do s <- get
                           return $ Str s
               5 ->     do n <- get
                           return $ NumInt n
               6 ->     do n <- get
                           return $ NumFloat n
               7 ->     do b <- get
                           return $ Bit b
               9 ->     do h <- get
                           return $ Hash h
               11 ->    do i <- get
                           e <- get
                           return $ Lambda i e
               12 ->    do p <- get
                           return $ Proc p
               13 ->    do th <- get
                           return $ Thread th
               14 ->    do return $ Null
               15 ->    do s <- get
                           return $ Undefined s
               16 ->    do f <- get
                           return $ File f


instance Binary(Expr) where
    put (Exception s) =     do put (18 :: Word8)
                               put s
    put (Skip) =            do put (19 :: Word8)
    put (Val v) =           do put (20 :: Word8)
                               put v
    put (List l) =          do put (21 :: Word8)
                               put l
    put (Take a b) =        do put (22 :: Word8)
                               put a
                               put b
    put (HashExpr h) =      do put (23 :: Word8)
                               put h
    put (Subs n s) =        do put (28 :: Word8)
                               put n
                               put s
    put (Add a b) =         do put (29 :: Word8)
                               put a
                               put b
    put (Sub a b) =         do put (30 :: Word8)
                               put a
                               put b
    put (Prod a b) =        do put (31 :: Word8)
                               put a
                               put b
    put (Div a b) =         do put (32 :: Word8)
                               put a
                               put b
    put (Mod a b) =         do put (33 :: Word8)
                               put a
                               put b
    put (Exp a b) =         do put (34 :: Word8)
                               put a
                               put b
    put (Eq a b) =          do put (35 :: Word8)
                               put a
                               put b
    put (InEq a b) =        do put (36 :: Word8)
                               put a
                               put b
    put (Gt a b) =          do put (37 :: Word8)
                               put a
                               put b
    put (Lt a b) =          do put (38 :: Word8)
                               put a
                               put b
    put (And a b) =         do put (39 :: Word8)
                               put a
                               put b
    put (Or a b) =          do put (40 :: Word8)
                               put a
                               put b
    put (Not a) =           do put (41 :: Word8)
                               put a
    put (Def a b c) =       do put (42 :: Word8)
                               put a
                               put b
                               put c
    put (EagerDef a b c) =  do put (43 :: Word8)
                               put a
                               put b
                               put c
    put (Concat a b) =      do put (44 :: Word8)
                               put a
                               put b
    put (Call f p) =        do put (46 :: Word8)
                               put f
                               put p
    put (Var f) =           do put (47 :: Word8)
                               put f
    put (If a b c) =        do put (48 :: Word8)
                               put a
                               put b
                               put c
    put (Case a b) =        do put (49 :: Word8)
                               put a
                               put b
    put (For a b c d) =     do put (50 :: Word8)
                               put a
                               put b
                               put c
                               put d
    put (Range a b c) =     do put (51 :: Word8)
                               put a
                               put b
                               put c
    put (Input) =           do put (53 :: Word8)
    put (Import a b) =      do put (54 :: Word8)
                               put a
                               put b
    put (FileObj a) =       do put (55 :: Word8)
                               put a
    put (FileRead a) =      do put (56 :: Word8)
                               put a
    put (FileWrite a b) =   do put (57 :: Word8)
                               put a
                               put b
    put (FileAppend a b) =  do put (58 :: Word8)
                               put a
                               put b
    put (EvalExpr a) =      do put (61 :: Word8)
                               put a
    put (Rule a) =          do put (62 :: Word8)
                               put a
    put (UseRule a b) =     do put (63 :: Word8)
                               put a
                               put b
    get = do t <- get :: Get Word8
             case t of 
               18 ->    do s <- get
                           return $ Exception s
               19 ->    do return Skip
               20 ->    do v <- get
                           return $ Val v
               21 ->    do l <- get
                           return $ List l
               22 ->    do a <- get
                           b <- get
                           return $ Take a b
               23 ->    do h <- get
                           return $ HashExpr h
               28 ->    do a <- get
                           b <- get
                           return $ Subs a b
               29 ->    do a <- get
                           b <- get
                           return $ Add a b
               30 ->    do a <- get
                           b <- get
                           return $ Sub a b
               31 ->    do a <- get
                           b <- get
                           return $ Prod a b
               32 ->    do a <- get
                           b <- get
                           return $ Div a b
               33 ->    do a <- get
                           b <- get
                           return $ Mod a b
               34 ->    do a <- get
                           b <- get
                           return $ Exp a b
               35 ->    do a <- get
                           b <- get
                           return $ Eq a b
               36 ->    do a <- get
                           b <- get
                           return $ InEq a b
               37 ->    do a <- get
                           b <- get
                           return $ Gt a b
               38 ->    do a <- get
                           b <- get
                           return $ Lt a b
               39 ->    do a <- get
                           b <- get
                           return $ And a b
               40 ->    do a <- get
                           b <- get
                           return $ Or a b
               41 ->    do a <- get
                           return $ Not a
               42 ->    do a <- get
                           b <- get
                           c <- get
                           return $ Def a b c
               43 ->    do a <- get
                           b <- get
                           c <- get
                           return $ EagerDef a b c
               44 ->    do a <- get
                           b <- get
                           return $ Concat a b
               46 ->    do a <- get
                           b <- get
                           return $ Call a b
               47 ->    do a <- get
                           return $ Var a
               48 ->    do a <- get
                           b <- get
                           c <- get
                           return $ If a b c
               49 ->    do a <- get
                           b <- get
                           return $ Case a b
               50 ->    do a <- get
                           b <- get
                           c <- get
                           d <- get
                           return $ For a b c d
               51 ->    do a <- get
                           b <- get
                           c <- get
                           return $ Range a b c
               53 ->    do return $ Input
               54 ->    do a <- get
                           b <- get
                           return $ Import a b
               55 ->    do a <- get
                           return $ FileObj a
               56 ->    do a <- get
                           return $ FileRead a
               57 ->    do a <- get
                           b <- get
                           return $ FileWrite a b
               58 ->    do a <- get
                           b <- get
                           return $ FileAppend a b
               61 ->    do a <- get
                           return $ EvalExpr a
               62 ->    do a <- get
                           return $ Rule a
               63 ->    do a <- get
                           b <- get
                           return $ UseRule a b
