{-# LANGUAGE CApiFFI #-}

module Pavel.Xnu.AttrList.AttrCmn where

import Control.Monad
import Data.Word
import System.Posix.Types

import Pavel.EnumBitFlags
import Pavel.SafeBuf
import Pavel.Xnu.Types
import Pavel.Xnu.AttrList.Peekery

#include <sys/attr.h>

{#enum define AttrCmn
  { ATTR_CMN_NAME as ATTR_CMN_NAME
  , ATTR_CMN_DEVID as ATTR_CMN_DEVID
  , ATTR_CMN_FSID as ATTR_CMN_FSID
  , ATTR_CMN_OBJTYPE as ATTR_CMN_OBJTYPE
  , ATTR_CMN_OBJTAG as ATTR_CMN_OBJTAG
  , ATTR_CMN_OBJID as ATTR_CMN_OBJID
  , ATTR_CMN_OBJPERMANENTID as ATTR_CMN_OBJPERMANENTID
  , ATTR_CMN_PAROBJID as ATTR_CMN_PAROBJID
  , ATTR_CMN_SCRIPT as ATTR_CMN_SCRIPT
  , ATTR_CMN_CRTIME as ATTR_CMN_CRTIME
  , ATTR_CMN_MODTIME as ATTR_CMN_MODTIME
  , ATTR_CMN_CHGTIME as ATTR_CMN_CHGTIME
  , ATTR_CMN_ACCTIME as ATTR_CMN_ACCTIME
  , ATTR_CMN_BKUPTIME as ATTR_CMN_BKUPTIME
  , ATTR_CMN_FNDRINFO as ATTR_CMN_FNDRINFO
  , ATTR_CMN_OWNERID as ATTR_CMN_OWNERID
  , ATTR_CMN_GRPID as ATTR_CMN_GRPID
  , ATTR_CMN_ACCESSMASK as ATTR_CMN_ACCESSMASK
  , ATTR_CMN_FLAGS as ATTR_CMN_FLAGS
  -- ATTR_CMN_GEN_COUNT requires FSOPT_ATTR_CMN_EXTENDED opt
  , ATTR_CMN_GEN_COUNT as ATTR_CMN_GEN_COUNT
  , ATTR_CMN_DOCUMENT_ID as ATTR_CMN_DOCUMENT_ID
  , ATTR_CMN_USERACCESS as ATTR_CMN_USERACCESS
  , ATTR_CMN_EXTENDED_SECURITY as ATTR_CMN_EXTENDED_SECURITY
  , ATTR_CMN_UUID as ATTR_CMN_UUID
  , ATTR_CMN_GRPUUID as ATTR_CMN_GRPUUID
  , ATTR_CMN_FILEID as ATTR_CMN_FILEID
  , ATTR_CMN_PARENTID as ATTR_CMN_PARENTID
  , ATTR_CMN_FULLPATH as ATTR_CMN_FULLPATH
  , ATTR_CMN_ADDEDTIME as ATTR_CMN_ADDEDTIME
  , ATTR_CMN_DATA_PROTECT_FLAGS as ATTR_CMN_DATA_PROTECT_FLAGS
  , ATTR_CMN_RETURNED_ATTRS as ATTR_CMN_RETURNED_ATTRS
  }
  deriving (Eq, Ord, Show)
#}

packAttrCmns :: [AttrCmn] -> Word32
packAttrCmns attrs =
  packEnumBitFlags (EnumBitFlags attrs)

data AttrCmnValue
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
  deriving (Show)

peekAttrCmn :: AttrCmn -> AttrListBuf -> IO (AttrListBuf, AttrCmnValue)
peekAttrCmn ATTR_CMN_NAME = peekStringAttr AttrCmnName
peekAttrCmn ATTR_CMN_DEVID = peekFixedAttr AttrCmnDevId
peekAttrCmn ATTR_CMN_FSID = peekFixedAttr AttrCmnFsId
peekAttrCmn ATTR_CMN_OBJTYPE =
  peekEnumAttr (undefined :: {#type fsobj_type_t#}) AttrCmnObjType
peekAttrCmn ATTR_CMN_OBJTAG =
  peekEnumAttr (undefined :: {#type fsobj_tag_t#}) AttrCmnObjTag
peekAttrCmn ATTR_CMN_OBJID = peekFixedAttr AttrCmnObjId
peekAttrCmn ATTR_CMN_OBJPERMANENTID = peekFixedAttr AttrCmnObjPermanentId
peekAttrCmn ATTR_CMN_PAROBJID = peekFixedAttr AttrCmnParObjId
peekAttrCmn ATTR_CMN_SCRIPT = peekFixedAttr AttrCmnScript
peekAttrCmn ATTR_CMN_CRTIME = peekFixedAttr AttrCmnCrTime
peekAttrCmn ATTR_CMN_MODTIME = peekFixedAttr AttrCmnModTime
peekAttrCmn ATTR_CMN_CHGTIME = peekFixedAttr AttrCmnChgTime
peekAttrCmn ATTR_CMN_ACCTIME = peekFixedAttr AttrCmnAccTime
peekAttrCmn ATTR_CMN_BKUPTIME = peekFixedAttr AttrCmnBkupTime
peekAttrCmn ATTR_CMN_FNDRINFO = peekFixedAttr AttrCmnFndrInfo
peekAttrCmn ATTR_CMN_OWNERID = peekFixedAttr AttrCmnOwnerId
peekAttrCmn ATTR_CMN_GRPID = peekFixedAttr AttrCmnGrpId
peekAttrCmn ATTR_CMN_ACCESSMASK = peekFixedAttr AttrCmnAccessMask
peekAttrCmn ATTR_CMN_FLAGS = peekFixedAttr AttrCmnFlags
peekAttrCmn ATTR_CMN_GEN_COUNT = peekFixedAttr AttrCmnGenCount
peekAttrCmn ATTR_CMN_DOCUMENT_ID = peekFixedAttr AttrCmnDocumentId
peekAttrCmn ATTR_CMN_EXTENDED_SECURITY =
  peekKauthAclAttr AttrCmnExtendedSecurity
peekAttrCmn _ = \_ -> ioError $ userError "attr not supported"

peekAttrCmns :: [AttrCmn] -> AttrListBuf -> IO [AttrCmnValue]
peekAttrCmns attrs buf = do
  (Buf _ mustBeZero, ret) <- foldM peekOne (buf, []) attrs
  if | mustBeZero > 0 -> do
         putStrLn $ (show mustBeZero) ++ " bytes left in buffer!!"
     | mustBeZero < 0 -> do
         putStrLn $ (show mustBeZero) ++ " bytes buffer overrun!!"
     | otherwise -> return ()
  return $ reverse ret
  where
    peekOne (buf', retAcc) attr = do
      (buf'', value) <- peekAttrCmn attr buf'
      return (buf'', value : retAcc)
