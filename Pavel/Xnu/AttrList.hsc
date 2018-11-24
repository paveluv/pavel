{-# LANGUAGE CApiFFI #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Pavel.Xnu.AttrList
-- Copyright   :  (c) Pavel Uvarov 2018
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  puvar@puvar.net
-- Stability   :  experimental
-- Portability :  non-portable (requires Darwin OS)
--
-- Support for getattrlist system call.
--
-----------------------------------------------------------------------------

module Pavel.Xnu.AttrList
  ( AttrList(..)
  , AttrCmn(..)
  , FsOpts
  , getAttrList
  ) where

import Data.List
import Foreign
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import System.Posix.Internals

import Pavel.EnumBitFlags
import Pavel.Xnu.AttrList.AttrCmn
import Pavel.Xnu.AttrList.Peekery
import Pavel.Xnu.Types

#include <sys/attr.h>
#include <unistd.h>

data AttrList = AttrList
  { commonAttr :: [AttrCmn]
  }

instance Storable AttrList where
  sizeOf _ = #{size struct attrlist}
  alignment _ = alignment (undefined :: Word32)
  poke ptr attrList = do
    #{poke struct attrlist, bitmapcount} ptr $ (#{const ATTR_BIT_MAP_COUNT} :: Word32)
    #{poke struct attrlist, commonattr} ptr $ packAttrCmns $ commonAttr attrList
    #{poke struct attrlist, volattr} ptr $ (0 :: Word32)
    #{poke struct attrlist, dirattr} ptr $ (0 :: Word32)
    #{poke struct attrlist, fileattr} ptr $ (0 :: Word32)
    #{poke struct attrlist, forkattr} ptr $ (0 :: Word32)
  peek _ = ioError $ userError "do not need yet"

withAttrList :: AttrList -> (Ptr AttrList -> IO a) -> IO a
withAttrList attrList f =
  allocaBytes (sizeOf attrList) $ \pAttrList -> do
    poke pAttrList attrList
    f pAttrList

foreign import capi unsafe "unistd.h getattrlist" c_getattrlist ::
     CString
  -> Ptr AttrList
  -> Ptr CChar
  -> CSize
  -> CULong
  -> IO CInt

type FsOpts = EnumBitFlags CULong FsOpt

getAttrList :: FsOpts -> AttrList -> FilePath -> IO [AttrCmnValue]
getAttrList opts attrList path =
  withFilePath path $ \cPath -> do
    withAttrList attrList $ \pAttrList -> do
      result <- go cPath pAttrList 8
      case result of
        Left size -> do
          putStrLn "buffer realloc"
          result' <- go cPath pAttrList size
          case result' of
            Left _ -> ioError $ userError "error after buffer realloc"
            Right ret -> return ret
        Right ret -> return ret
  where
    go cPath pAttrList bufSize = do
      fpAttrBuf <- mallocForeignPtrBytes bufSize
      withForeignPtr fpAttrBuf $ \pAttrBuf -> do
        throwErrnoPathIfMinus1_
          "getAttrList"
          path
          (c_getattrlist
            cPath pAttrList pAttrBuf (toEnum bufSize) (foldEnumBitFlags opts))
        (size :: Int) <-
          (fromIntegral <$> peek ((castPtr pAttrBuf) :: Ptr Word32))
        if size > bufSize
          then
            Left <$> return size
          else
            Right <$> peekAttrCmns
              (sort $ commonAttr attrList)
              ((pAttrBuf, size) `advanceBuf` 4)
