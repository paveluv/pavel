{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
  ( Attr(..)
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
import Pavel.Xnu.Types

#include <sys/types.h>
#include <sys/acl.h>
#include <sys/attr.h>
#include <unistd.h>

-- | Enum of all attributes.

data Attr =
    ATTR_CMN_RETURNED_ATTRS
  | ATTR_CMN_NAME
  | ATTR_CMN_DEVID
  | ATTR_CMN_FSID
  | ATTR_CMN_OBJTYPE
  | ATTR_CMN_OBJTAG
  | ATTR_CMN_OBJID
  | ATTR_CMN_OBJPERMANENTID
  | ATTR_CMN_PAROBJID
  | ATTR_CMN_SCRIPT
  | ATTR_CMN_CRTIME
  | ATTR_CMN_MODTIME
  | ATTR_CMN_CHGTIME
  | ATTR_CMN_ACCTIME
  | ATTR_CMN_BKUPTIME
  | ATTR_CMN_FNDRINFO
  | ATTR_CMN_OWNERID
  | ATTR_CMN_GRPID
  | ATTR_CMN_ACCESSMASK
  | ATTR_CMN_FLAGS
  -- ATTR_CMN_GEN_COUNT requires FSOPT_ATTR_CMN_EXTENDED opt
  | ATTR_CMN_GEN_COUNT
  | ATTR_CMN_DOCUMENT_ID
  | ATTR_CMN_USERACCESS
  | ATTR_CMN_EXTENDED_SECURITY
  | ATTR_CMN_UUID
  | ATTR_CMN_GRPUUID
  | ATTR_CMN_FILEID
  | ATTR_CMN_PARENTID
  | ATTR_CMN_FULLPATH
  | ATTR_CMN_ADDEDTIME
  | ATTR_CMN_DATA_PROTECT_FLAGS
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | Attribute value data type.

data AttrValue
  = AttrCmnReturnedAttrs AttributeSet
  | AttrCmnName String
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
  | AttrCmnUserAccess Word32
  | AttrCmnExtendedSecurity (Maybe KauthAcl)
  | AttrCmnUuid Guid
  | AttrCmnGrpUuid Guid
  | AttrCmnFileId CIno
  | AttrCmnParentId CIno
  | AttrCmnFullPath String
  | AttrCmnAddedTime TimeSpec
  | AttrCmnDataProtectFlags Word32
  deriving (Show)

-- All objects in attrlist buffer are aligned to 4 bytes.
type AlignmentType = Word32

-------------------------------------------------------------------------------
-- Attribute configuration.
-------------------------------------------------------------------------------

-- | Corresponds to 4 fields of @struct attrlist@.
-- @AttrFork@ and @AttrExtCmn@ share the same @forkattr@ field which is
-- controlled by @FSOPT_ATTR_CMN_EXTENDED@ option.
data AttrKind =
    AttrCmn
  | AttrVol
  | AttrDir
  | AttrFile
  | AttrFork
  | AttrExtCmn

type AttrBitRep = Word32

-- | Used to calculate max buffer size to pass to @getattrlist@ system call.
type AttrMaxSize = Int

type AttrPeekFn = AttrListBuf -> IO (AttrListBuf, AttrValue)

type AttrConf = (AttrKind, AttrBitRep, AttrMaxSize, AttrPeekFn)

alignedSize :: Int -> Int
alignedSize sz = (n + sz) .&. (complement n)
  where
    n = sizeOf (undefined :: AlignmentType)

fixedAttrSize :: forall v. (Storable v) => (v -> AttrValue) -> AttrMaxSize
fixedAttrSize _ = alignedSize $ sizeOf (undefined :: v)

fixedAttr :: forall v.
     (Storable v)
  => AttrKind
  -> AttrBitRep
  -> (v -> AttrValue)
  -> AttrConf
fixedAttr kind bitRep constr =
  (kind, bitRep, fixedAttrSize constr, peekFixedAttr constr)

attrReferenceSize :: Int
attrReferenceSize = alignedSize #{size struct attrreference}

-- | Mapping between Attr enum and meta info required to peek attributes
-- from the buffer.
attrConf :: Attr -> AttrConf
attrConf ATTR_CMN_RETURNED_ATTRS =
  ( AttrCmn
  , #{const ATTR_CMN_RETURNED_ATTRS}
  , #{size attribute_set_t}
  , undefined
  )
attrConf ATTR_CMN_NAME =
  ( AttrCmn
  , #{const ATTR_CMN_NAME}
  , attrReferenceSize + alignedSize (3 * #{const NAME_MAX} + 1)
  , peekStringAttr AttrCmnName
  )
attrConf ATTR_CMN_DEVID =
  fixedAttr AttrCmn #{const ATTR_CMN_DEVID} AttrCmnDevId
attrConf ATTR_CMN_FSID =
  fixedAttr AttrCmn #{const ATTR_CMN_FSID} AttrCmnFsId
attrConf ATTR_CMN_OBJTYPE =
  ( AttrCmn
  , #{const ATTR_CMN_OBJTYPE}
  , #{size fsobj_type_t}
  , peekEnumAttr (undefined :: #{type fsobj_type_t}) AttrCmnObjType
  )
attrConf ATTR_CMN_OBJTAG =
  ( AttrCmn
  , #{const ATTR_CMN_OBJTAG}
  , #{size fsobj_tag_t}
  , peekEnumAttr (undefined :: #{type fsobj_tag_t}) AttrCmnObjTag
  )
attrConf ATTR_CMN_OBJID =
  fixedAttr AttrCmn #{const ATTR_CMN_OBJID} AttrCmnObjId
attrConf ATTR_CMN_OBJPERMANENTID =
  fixedAttr AttrCmn #{const ATTR_CMN_OBJPERMANENTID} AttrCmnObjPermanentId
attrConf ATTR_CMN_PAROBJID =
  fixedAttr AttrCmn #{const ATTR_CMN_PAROBJID} AttrCmnParObjId
attrConf ATTR_CMN_SCRIPT =
  fixedAttr AttrCmn #{const ATTR_CMN_SCRIPT} AttrCmnScript
attrConf ATTR_CMN_CRTIME =
  fixedAttr AttrCmn #{const ATTR_CMN_CRTIME} AttrCmnCrTime
attrConf ATTR_CMN_MODTIME =
  fixedAttr AttrCmn #{const ATTR_CMN_MODTIME} AttrCmnModTime
attrConf ATTR_CMN_CHGTIME =
  fixedAttr AttrCmn #{const ATTR_CMN_CHGTIME} AttrCmnChgTime
attrConf ATTR_CMN_ACCTIME =
  fixedAttr AttrCmn #{const ATTR_CMN_ACCTIME} AttrCmnAccTime
attrConf ATTR_CMN_BKUPTIME =
  fixedAttr AttrCmn #{const ATTR_CMN_BKUPTIME} AttrCmnBkupTime
attrConf ATTR_CMN_FNDRINFO =
  fixedAttr AttrCmn #{const ATTR_CMN_FNDRINFO} AttrCmnFndrInfo
attrConf ATTR_CMN_OWNERID =
  fixedAttr AttrCmn #{const ATTR_CMN_OWNERID} AttrCmnOwnerId
attrConf ATTR_CMN_GRPID =
  fixedAttr AttrCmn #{const ATTR_CMN_GRPID} AttrCmnGrpId
attrConf ATTR_CMN_ACCESSMASK =
  fixedAttr AttrCmn #{const ATTR_CMN_ACCESSMASK} AttrCmnAccessMask
attrConf ATTR_CMN_FLAGS =
  fixedAttr AttrCmn #{const ATTR_CMN_FLAGS} AttrCmnFlags
attrConf ATTR_CMN_GEN_COUNT =
  fixedAttr AttrCmn #{const ATTR_CMN_GEN_COUNT} AttrCmnGenCount
attrConf ATTR_CMN_DOCUMENT_ID =
  fixedAttr AttrCmn #{const ATTR_CMN_DOCUMENT_ID} AttrCmnDocumentId
attrConf ATTR_CMN_USERACCESS =
  fixedAttr AttrCmn #{const ATTR_CMN_USERACCESS} AttrCmnUserAccess
attrConf ATTR_CMN_EXTENDED_SECURITY =
  ( AttrCmn
  , #{const ATTR_CMN_EXTENDED_SECURITY}
  , attrReferenceSize +
    alignedSize
      (#{size struct kauth_filesec} +
        (#{const KAUTH_ACL_MAX_ENTRIES} - 1) * #{size struct kauth_ace})
  ,
  peekKauthAclAttr AttrCmnExtendedSecurity
  )
attrConf ATTR_CMN_UUID =
  fixedAttr AttrCmn #{const ATTR_CMN_UUID} AttrCmnUuid
attrConf ATTR_CMN_GRPUUID =
  fixedAttr AttrCmn #{const ATTR_CMN_GRPUUID} AttrCmnGrpUuid
attrConf ATTR_CMN_FILEID =
  fixedAttr AttrCmn #{const ATTR_CMN_FILEID} AttrCmnFileId
attrConf ATTR_CMN_PARENTID =
  fixedAttr AttrCmn #{const ATTR_CMN_PARENTID} AttrCmnParentId
attrConf ATTR_CMN_FULLPATH =
  ( AttrCmn
  , #{const ATTR_CMN_FULLPATH}
  , attrReferenceSize + alignedSize (3 * #{const PATH_MAX} + 1)
  , peekStringAttr AttrCmnFullPath
  )
attrConf ATTR_CMN_ADDEDTIME =
  fixedAttr AttrCmn #{const ATTR_CMN_ADDEDTIME} AttrCmnAddedTime
attrConf ATTR_CMN_DATA_PROTECT_FLAGS =
  fixedAttr
    AttrCmn
    #{const ATTR_CMN_DATA_PROTECT_FLAGS}
    AttrCmnDataProtectFlags
attrConf attr = error ("attr " ++ (show attr) ++ " not supported yet")

type AttrConfList = [(Attr, AttrConf)]

attrConfList :: AttrConfList
attrConfList = [(attr, attrConf attr) | attr <- [minBound..]]

cmnBitReps = [(attr, bitrep) | (attr, (AttrCmn, bitrep, _, _)) <- attrConfList]

-------------------------------------------------------------------------------
-- Peek functions.
-------------------------------------------------------------------------------

type AttrListBuf = Buf AlignmentType

newtype AttributeSet = AttributeSet [Attr] deriving newtype (Show)

peekAttributeSet :: Bool -> AttrListBuf -> IO (AttrListBuf, AttributeSet)
peekAttributeSet cmnExt buf@(Buf ptr _) = do
  buf' <- safeAdvance buf #{size attribute_set_t}
  (commonattr :: Word32) <-
    #{peek attribute_set_t, commonattr} (castPtr ptr)
  let lst =
        [attr | (attr, br) <- cmnBitReps, br .&. commonattr /= (toEnum 0)]
   in return (buf', AttributeSet lst)

-- | Peek something and wrap it in an AttrValue constructor.
peekFixedAttr ::
     (Storable v)
  => (v -> AttrValue)
  -> AttrListBuf
  -> IO (AttrListBuf, AttrValue)
peekFixedAttr valueConstr buf = do
  (buf', value) <- safePeek buf
  return (buf', valueConstr value)

-- | Peek an enum stored as type c (some enums are stored as Word16 and some as
-- Word32). Note, the pointer will be advanced according to buf type @w@
-- anyway.
peekEnumAttr ::
     forall v w. (Enum v, Enum w, Storable w)
  => w
  -> (v -> AttrValue)
  -> AttrListBuf
  -> IO (AttrListBuf, AttrValue)
peekEnumAttr _ valueConstr buf = do
  (buf', value :: w) <- safePeek buf
  return (buf', valueConstr $ toEnum $ fromEnum value)

-- | Peek a @attreference@ structure and return a buffer holding the area it
-- points to. The original buffer is advanced to the next entry.
peekAttrReference :: AttrListBuf -> IO (AttrListBuf, AttrListBuf)
peekAttrReference buf@(Buf ptr size) = do
  (buf', off :: Int32) <- safePeek buf
  (buf'', len :: Word32) <- safePeek buf'
  let iOff = fromEnum off
      iLen = fromEnum len
   in if iOff + iLen > size
        then error "attrreference out of buffer"
        else return (buf'', Buf (ptr `plusPtr` iOff) iLen)

-- | Peek a NUL-terminated string referenced by @attreference@ and wrap it
-- into the provided constructor. The terminating NUL character is removed.
peekStringAttr :: (String -> a) -> AttrListBuf -> IO (AttrListBuf, a)
peekStringAttr valueConstr buf = do
  (buf', Buf strPtr strLen) <- peekAttrReference buf
  str <- peekCStringLen (castPtr strPtr, strLen - 1)
  return (buf', valueConstr str)

-- | Peek a variable size @struct kauth_filesec@ and keep only the acl part.
-- Note, that if the extended security is missing, the @attrreference@
-- structure return length 0, so this case is handled by Maybe.
peekKauthAclAttr ::
    (Maybe KauthAcl -> a)
  -> AttrListBuf
  -> IO (AttrListBuf, a)
peekKauthAclAttr valueConstr buf = do
  (buf', filesecBuf@(Buf _ size)) <- peekAttrReference buf
  if size == 0
    then return (buf', valueConstr Nothing)
    else do
      (_, KauthFilesec {fsec_acl = acl}) <- peekKauthFilesec filesecBuf
      return (buf', valueConstr $ Just acl)

peekAttrs :: [Attr] -> AttrListBuf -> IO [AttrValue]
peekAttrs attrs buf =
  case attrs of
    ATTR_CMN_RETURNED_ATTRS : rest -> do
      (buf', attrSet@(AttributeSet attrs')) <-
        peekAttributeSet True buf
      putStrLn (show attrs')
      putStrLn (show [attr | attr <- attrs, not (attr `elem` attrs')])
      (AttrCmnReturnedAttrs attrSet:) <$> go buf' (tail attrs')
    _ ->
      go buf attrs
  where
    go buf' attrs' = do
      (_, ret) <- foldM peekOne (buf', []) attrs'
      return $ reverse ret
      where
        peekOne (buf'', retAcc) attr =
          let (_, _, _, fn) = attrConf attr
           in do
                (buf''', value) <- fn buf''
                return (buf''', value : retAcc)

-------------------------------------------------------------------------------
-- System call wrappers.
-------------------------------------------------------------------------------

-- | Attr list storable as @struct attrlist@ from @sys/attr.h@. Used as an
-- argument to @getattrlist@ system call.
data AttrList = AttrList [Attr]

instance Storable AttrList where
  sizeOf _ = #{size struct attrlist}
  alignment _ = 4
  poke ptr (AttrList attrs) = do
    #{poke struct attrlist, bitmapcount} ptr $
      (#{const ATTR_BIT_MAP_COUNT} :: Word32)
    #{poke struct attrlist, commonattr} ptr $
      foldr (.|.) 0 [bitRep | (AttrCmn, bitRep, _, _) <- map attrConf attrs]
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
getAttrList :: FsOpts -> [Attr] -> FilePath -> IO [AttrValue]
getAttrList opts attrs path =
  withFilePath path $ \cPath -> do
    withAttrList (AttrList attrs) $ \pAttrList -> do
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
            Right <$> peekAttrs
              (sort attrs)
              (Buf (pAttrBuf `plusPtr` 4) (size - 4))
