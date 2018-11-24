{-# LANGUAGE CApiFFI #-}

module Pavel.Xnu.Xattr where

import Data.Bits
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Ptr
import System.Posix.Internals
import System.Posix.Types

#include <sys/xattr.h>

foreign import capi unsafe "sys/xattr.h listxattr" c_listxattr
  :: CString -> Ptr CChar -> CSize -> CInt -> IO CSsize

data XattrFlag
  = XATTR_NOFOLLOW
  | XATTR_SHOWCOMPRESSION

packXattrFlag :: XattrFlag -> CInt
packXattrFlag XATTR_NOFOLLOW = #const XATTR_NOFOLLOW
packXattrFlag XATTR_SHOWCOMPRESSION = #const XATTR_SHOWCOMPRESSION

packXattrFlags :: [XattrFlag] -> CInt
packXattrFlags flags = foldl (\s f -> (packXattrFlag f) .|. s) 0 flags

hasXattr :: [XattrFlag] -> FilePath -> IO CSsize
hasXattr flags path =
  withFilePath path $ \p -> do
    size <-
      throwErrnoPathIfMinus1
        "hasXattr"
        path
        (c_listxattr p nullPtr 0 (packXattrFlags flags))
    return $ size

getXattr :: [XattrFlag] -> FilePath -> IO String
getXattr flags path =
  withFilePath path $ \p -> do
    buf <- mallocForeignPtrBytes 64
    withForeignPtr buf $ \bufp -> do
      size <-
        throwErrnoPathIfMinus1
          "hasXattr"
          path
          (c_listxattr p bufp (64 :: CSize) (packXattrFlags flags))
      if size > 64
        then do
          buf2 <- mallocForeignPtrBytes (fromIntegral size)
          withForeignPtr buf2 $ \bufp2 -> do
            throwErrnoPathIfMinus1_
              "hasXattr"
              path
              (c_listxattr p bufp2 (fromIntegral size) (packXattrFlags flags))
            peekCStringLen (bufp2, fromIntegral size)
        else peekCStringLen (bufp, fromIntegral size)
