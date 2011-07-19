{-# LANGUAGE ForeignFunctionInterface #-}
module Scotch.Types.FFI where

import Foreign
import Foreign.C

foreign import ccall "dynamic" mkPtrFun :: FunPtr (Ptr a -> IO (Ptr a)) -> Ptr a -> IO (Ptr a)

callWithNull :: FunPtr (Ptr a -> IO (Ptr a)) -> IO (Ptr a)
callWithNull f = mkPtrFun f nullPtr
