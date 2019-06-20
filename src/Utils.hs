module Utils (
    intToPtr
  ) where

import Foreign.Ptr(Ptr, IntPtr(..), intPtrToPtr)
import Foreign.Marshal.Array(withArray)
import Foreign.Storable

intToPtr :: Int -> Ptr a
intToPtr = intPtrToPtr . IntPtr
