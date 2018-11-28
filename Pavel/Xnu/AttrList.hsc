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
  , CommonAttr(..)
  , FsOpts
  , getAttrList
  ) where

import Control.Monad
import Data.List
import Data.Word
import Foreign
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import System.Posix.Internals
import System.Posix.Types

import Pavel.EnumBitFlags
import Pavel.SafeBuf
import Pavel.Xnu.AttrList.CEnums
import Pavel.Xnu.AttrList.Peekery
import Pavel.Xnu.Types

#include <sys/attr.h>
#include <unistd.h>

data AttrValue
  = AttrCmnName String
  | AttrCmnDevId CDev
  | AttrCmnFsId FsId
  | AttrCmnObjType VType
  | AttrCmnObjTag VTagType
  | AttrCmnObjId FsObjId
  | AttrCmnObjPermanentId FsObjId
  | AttrCmnParObjId FsObjId
  | AttrCmnScript TextEncoding
  | AttrCmnCrTime TimeSpec
  | AttrCmnModTime TimeSpec
  | AttrCmnChgTime TimeSpec
  | AttrCmnAccTime TimeSpec
  | AttrCmnBkupTime TimeSpec
  | AttrCmnFndrInfo RawFinderInfo
  | AttrCmnOwnerId CUid
  | AttrCmnGrpId CGid
  | AttrCmnAccessMask CMode
  | AttrCmnFlags FileFlags
  | AttrCmnGenCount Word32
  | AttrCmnDocumentId Word32
  | AttrCmnExtendedSecurity (Maybe KauthAcl)
  | AttrCmnUuid Guid
  | AttrCmnGrpUuid Guid
  | AttrCmnFileId CIno
  | AttrCmnParentId CIno
  | AttrCmnFullPath String
  | AttrCmnAddedTime TimeSpec
  | AttrCmnDataProtectFlags Word32
  deriving (Show)

peekCommonAttr :: CommonAttr -> AttrListBuf -> IO (AttrListBuf, AttrValue)
peekCommonAttr ATTR_CMN_NAME = peekStringAttr AttrCmnName
peekCommonAttr ATTR_CMN_DEVID = peekFixedAttr AttrCmnDevId
peekCommonAttr ATTR_CMN_FSID = peekFixedAttr AttrCmnFsId
peekCommonAttr ATTR_CMN_OBJTYPE =
  peekEnumAttr (undefined :: #{type fsobj_type_t}) AttrCmnObjType
peekCommonAttr ATTR_CMN_OBJTAG =
  peekEnumAttr (undefined :: #{type fsobj_tag_t}) AttrCmnObjTag
peekCommonAttr ATTR_CMN_OBJID = peekFixedAttr AttrCmnObjId
peekCommonAttr ATTR_CMN_OBJPERMANENTID = peekFixedAttr AttrCmnObjPermanentId
peekCommonAttr ATTR_CMN_PAROBJID = peekFixedAttr AttrCmnParObjId
peekCommonAttr ATTR_CMN_SCRIPT = peekFixedAttr AttrCmnScript
peekCommonAttr ATTR_CMN_CRTIME = peekFixedAttr AttrCmnCrTime
peekCommonAttr ATTR_CMN_MODTIME = peekFixedAttr AttrCmnModTime
peekCommonAttr ATTR_CMN_CHGTIME = peekFixedAttr AttrCmnChgTime
peekCommonAttr ATTR_CMN_ACCTIME = peekFixedAttr AttrCmnAccTime
peekCommonAttr ATTR_CMN_BKUPTIME = peekFixedAttr AttrCmnBkupTime
peekCommonAttr ATTR_CMN_FNDRINFO = peekFixedAttr AttrCmnFndrInfo
peekCommonAttr ATTR_CMN_OWNERID = peekFixedAttr AttrCmnOwnerId
peekCommonAttr ATTR_CMN_GRPID = peekFixedAttr AttrCmnGrpId
peekCommonAttr ATTR_CMN_ACCESSMASK = peekFixedAttr AttrCmnAccessMask
peekCommonAttr ATTR_CMN_FLAGS = peekFixedAttr AttrCmnFlags
peekCommonAttr ATTR_CMN_GEN_COUNT = peekFixedAttr AttrCmnGenCount
peekCommonAttr ATTR_CMN_DOCUMENT_ID = peekFixedAttr AttrCmnDocumentId
peekCommonAttr ATTR_CMN_EXTENDED_SECURITY =
  peekKauthAclAttr AttrCmnExtendedSecurity
peekCommonAttr ATTR_CMN_UUID = peekFixedAttr AttrCmnUuid
peekCommonAttr ATTR_CMN_GRPUUID = peekFixedAttr AttrCmnGrpUuid
peekCommonAttr ATTR_CMN_FILEID = peekFixedAttr AttrCmnFileId
peekCommonAttr ATTR_CMN_PARENTID = peekFixedAttr AttrCmnParentId
peekCommonAttr ATTR_CMN_FULLPATH = peekStringAttr AttrCmnFullPath
peekCommonAttr ATTR_CMN_ADDEDTIME = peekFixedAttr AttrCmnAddedTime
peekCommonAttr ATTR_CMN_DATA_PROTECT_FLAGS =
  peekFixedAttr AttrCmnDataProtectFlags
peekCommonAttr _ = \_ -> error "attr not supported"

peekCommonAttrs :: [CommonAttr] -> AttrListBuf -> IO [AttrValue]
peekCommonAttrs attrs buf = do
  (_, ret) <- foldM peekOne (buf, []) attrs
  return $ reverse ret
  where
    peekOne (buf', retAcc) attr = do
      (buf'', value) <- peekCommonAttr attr buf'
      return (buf'', value : retAcc)

-- | @struct attrlist@ from @sys/attr.h@. Used as an argument to @getattrlist@
-- system call.
-- Only common attributes for now...
data AttrList = AttrList
  { commonAttr :: [CommonAttr]
  }

instance Storable AttrList where
  sizeOf _ = #{size struct attrlist}
  alignment _ = 4
  poke ptr attrList = do
    #{poke struct attrlist, bitmapcount} ptr $
      (#{const ATTR_BIT_MAP_COUNT} :: Word32)
    #{poke struct attrlist, commonattr} ptr $
      ((EnumBitFlags $ commonAttr attrList) :: EnumBitFlags Word32 CommonAttr)
    #{poke struct attrlist, volattr} ptr $ (0 :: Word32)
    #{poke struct attrlist, dirattr} ptr $ (0 :: Word32)
    #{poke struct attrlist, fileattr} ptr $ (0 :: Word32)
    #{poke struct attrlist, forkattr} ptr $ (0 :: Word32)
  peek _ = error "do not need yet"

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

-- | Make @getattrlist@ system call, and return interpreted results.
getAttrList :: FsOpts -> AttrList -> FilePath -> IO [AttrValue]
getAttrList opts attrList path =
  withFilePath path $ \cPath -> do
    withAttrList attrList $ \pAttrList -> do
      result <- go cPath pAttrList 8
      case result of
        Left size -> do
          result' <- go cPath pAttrList size
          case result' of
            Left _ -> error "error after buffer realloc"
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
            cPath pAttrList pAttrBuf (toEnum bufSize) (packEnumBitFlags opts))
        (size :: Int) <-
          (fromIntegral <$> peek ((castPtr pAttrBuf) :: Ptr Word32))
        if size > bufSize
          then
            Left <$> return size
          else
            Right <$> peekCommonAttrs
              (sort $ commonAttr attrList)
              (Buf (pAttrBuf `plusPtr` 4) (size - 4))
