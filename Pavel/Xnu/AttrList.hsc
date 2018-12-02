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
  ( Attr(..)
  , FsOpts
  , findValidArgs
  , getAttrList
  ) where

import Control.Exception
import Control.Monad
import Data.Array
import Data.List
import Data.Word
import Foreign
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import GHC.IO.Exception
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
  | ATTR_VOL_INFO
  | ATTR_VOL_FSTYPE
  | ATTR_VOL_SIGNATURE
  | ATTR_VOL_SIZE
  | ATTR_VOL_SPACEFREE
  | ATTR_VOL_SPACEAVAIL
  | ATTR_VOL_MINALLOCATION
  | ATTR_VOL_ALLOCATIONCLUMP
  | ATTR_VOL_IOBLOCKSIZE
  | ATTR_VOL_OBJCOUNT
  | ATTR_VOL_FILECOUNT
  | ATTR_VOL_DIRCOUNT
  | ATTR_VOL_MAXOBJCOUNT
  | ATTR_VOL_MOUNTPOINT
  | ATTR_VOL_NAME
  | ATTR_VOL_MOUNTFLAGS
  | ATTR_VOL_MOUNTEDDEVICE
  | ATTR_VOL_ENCODINGSUSED
--  | ATTR_VOL_CAPABILITIES
  | ATTR_VOL_UUID
  | ATTR_VOL_QUOTA_SIZE
  | ATTR_VOL_RESERVED_SIZE
--  | ATTR_VOL_ATTRIBUTES
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | Attribute value data type.

data AttrValue
  = AttrVoid
  -- * Common attributes.
  | AttrCmnReturnedAttrs AttributeSet
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
  -- * Volume attributes.
  | AttrVolFsType Word32
  | AttrVolSignature Word32
  | AttrVolSize COff
  | AttrVolSpaceFree COff
  | AttrVolSpaceAvail COff
  | AttrVolMinAllocation COff
  | AttrVolAllocationClump COff
  | AttrVolIoBlockSize Word32
  | AttrVolObjCount Word32
  | AttrVolFileCount Word32
  | AttrVolDirCount Word32
  | AttrVolMaxObjCount Word32
  | AttrVolMountPoint String
  | AttrVolName String
  | AttrVolMountFlags Word32
  | AttrVolMountedDevice String
  | AttrVolEncodingsUsed #{type unsigned long long}
--  | AttrVolCapabilities VolCapabilities
  | AttrVolUuid Guid
  | AttrVolQuotaSize COff
  | AttrVolReservedSize COff
--  | AttrVolAttributes VolAttributes
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
  deriving (Eq)

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

alignedNameMax :: Int
alignedNameMax = alignedSize (3 * #{const NAME_MAX} + 1)

alignedPathMax :: Int
alignedPathMax = alignedSize (3 * #{const PATH_MAX} + 1)

-- The same as PATH_MAX, but @/usr/include@ uses both...
alignedMaxPathLen :: Int
alignedMaxPathLen = alignedSize (3 * #{const MAXPATHLEN} + 1)

-- | Mapping between Attr enum and meta info required to peek attributes
-- from the buffer.
attrConf :: Attr -> AttrConf
--
-- Common Attrs
--
attrConf ATTR_CMN_RETURNED_ATTRS =
  ( AttrCmn
  , #{const ATTR_CMN_RETURNED_ATTRS}
  , #{size attribute_set_t}
  , undefined
  )
attrConf ATTR_CMN_NAME =
  ( AttrCmn
  , #{const ATTR_CMN_NAME}
  , attrReferenceSize + alignedNameMax
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
  , attrReferenceSize + alignedPathMax
  , peekStringAttr AttrCmnFullPath
  )
attrConf ATTR_CMN_ADDEDTIME =
  fixedAttr AttrCmn #{const ATTR_CMN_ADDEDTIME} AttrCmnAddedTime
attrConf ATTR_CMN_DATA_PROTECT_FLAGS =
  fixedAttr
    AttrCmn
    #{const ATTR_CMN_DATA_PROTECT_FLAGS}
    AttrCmnDataProtectFlags
--
-- Volume attrs
--
attrConf ATTR_VOL_INFO =
  ( AttrVol
  , #{const ATTR_VOL_INFO}
  , 0
  , (\buf -> return (buf, AttrVoid))
  )
attrConf ATTR_VOL_FSTYPE =
  fixedAttr AttrVol #{const ATTR_VOL_FSTYPE} AttrVolFsType
attrConf ATTR_VOL_SIGNATURE =
  fixedAttr AttrVol #{const ATTR_VOL_SIGNATURE} AttrVolSignature
attrConf ATTR_VOL_SIZE =
  fixedAttr AttrVol #{const ATTR_VOL_SIZE} AttrVolSize
attrConf ATTR_VOL_SPACEFREE =
  fixedAttr AttrVol #{const ATTR_VOL_SPACEFREE} AttrVolSpaceFree
attrConf ATTR_VOL_SPACEAVAIL =
  fixedAttr AttrVol #{const ATTR_VOL_SPACEAVAIL} AttrVolSpaceAvail
attrConf ATTR_VOL_MINALLOCATION =
  fixedAttr AttrVol #{const ATTR_VOL_MINALLOCATION} AttrVolMinAllocation
attrConf ATTR_VOL_ALLOCATIONCLUMP =
  fixedAttr AttrVol #{const ATTR_VOL_ALLOCATIONCLUMP} AttrVolAllocationClump
attrConf ATTR_VOL_IOBLOCKSIZE =
  fixedAttr AttrVol #{const ATTR_VOL_IOBLOCKSIZE} AttrVolIoBlockSize
attrConf ATTR_VOL_OBJCOUNT =
  fixedAttr AttrVol #{const ATTR_VOL_OBJCOUNT} AttrVolObjCount
attrConf ATTR_VOL_FILECOUNT =
  fixedAttr AttrVol #{const ATTR_VOL_FILECOUNT} AttrVolFileCount
attrConf ATTR_VOL_DIRCOUNT =
  fixedAttr AttrVol #{const ATTR_VOL_DIRCOUNT} AttrVolDirCount
attrConf ATTR_VOL_MAXOBJCOUNT =
  fixedAttr AttrVol #{const ATTR_VOL_MAXOBJCOUNT} AttrVolMaxObjCount
attrConf ATTR_VOL_MOUNTPOINT =
  ( AttrVol
  , #{const ATTR_VOL_MOUNTPOINT}
  , attrReferenceSize + alignedMaxPathLen
  , peekStringAttr AttrVolMountPoint
  )
attrConf ATTR_VOL_NAME =
  ( AttrVol
  , #{const ATTR_VOL_NAME}
  , attrReferenceSize + alignedNameMax
  , peekStringAttr AttrVolName
  )
attrConf ATTR_VOL_MOUNTFLAGS =
  fixedAttr AttrVol #{const ATTR_VOL_MOUNTFLAGS} AttrVolMountFlags
attrConf ATTR_VOL_MOUNTEDDEVICE =
  ( AttrVol
  , #{const ATTR_VOL_MOUNTEDDEVICE}
  , attrReferenceSize + alignedMaxPathLen
  , peekStringAttr AttrVolMountedDevice
  )
attrConf ATTR_VOL_ENCODINGSUSED =
  fixedAttr AttrVol #{const ATTR_VOL_ENCODINGSUSED} AttrVolEncodingsUsed
--attrConf ATTR_VOL_CAPABILITIES =
--  fixedAttr AttrVol #{const ATTR_VOL_CAPABILITIES} AttrVolCapabilities
attrConf ATTR_VOL_UUID =
  fixedAttr AttrVol #{const ATTR_VOL_UUID} AttrVolUuid
attrConf ATTR_VOL_QUOTA_SIZE =
  fixedAttr AttrVol #{const ATTR_VOL_QUOTA_SIZE} AttrVolQuotaSize
attrConf ATTR_VOL_RESERVED_SIZE =
  fixedAttr AttrVol #{const ATTR_VOL_RESERVED_SIZE} AttrVolReservedSize
--attrConf ATTR_VOL_ATTRIBUTES =
--  fixedAttr AttrVol #{const ATTR_VOL_ATTRIBUTES} AttrVolAttributes

attrConf attr = error ("attr " ++ (show attr) ++ " not supported yet")

type AttrConfList = [(Attr, AttrConf)]

attrConfList :: AttrConfList
attrConfList = [(attr, attrConf attr) | attr <- [minBound..]]

attrBitReps :: AttrKind -> [(Attr, AttrBitRep)]
attrBitReps kind =
  [(attr, bitrep) | (attr, (k, bitrep, _, _)) <- attrConfList, k == kind]

-- | Map from attr bit to Attr for fast lookup.
type AttrArray = Array Int Attr

attrArray :: AttrKind -> Array Int Attr
attrArray kind =
  array
    (0, finiteBitSize (undefined :: AttrBitRep) - 1)
    [(countTrailingZeros bitrep, attr) | (attr, bitrep) <- attrBitReps kind]

-------------------------------------------------------------------------------
-- Peek functions.
-------------------------------------------------------------------------------

type AttrListBuf = Buf AlignmentType

newtype AttributeSet = AttributeSet [Attr] deriving newtype (Show)

peekAttributeSet :: Bool -> AttrListBuf -> IO (AttrListBuf, AttributeSet)
peekAttributeSet cmnExt buf@(Buf ptr _) = do
  buf' <- safeAdvance buf #{size attribute_set_t}
  (commonattr :: AttrBitRep) <-
    #{peek attribute_set_t, commonattr} (castPtr ptr)
  (volattr :: AttrBitRep) <-
    #{peek attribute_set_t, volattr} (castPtr ptr)
  let lst = concatMap
              lstForKind
              [ (commonattr, AttrCmn)
              , (volattr, AttrVol)
              ]
 --          sort $ collectBits [] commonattr (attrArray AttrCmn)
   in do
        -- putStrLn ("attribute_set " ++ (show lst))
        return (buf', AttributeSet lst)
  where
    lstForKind (w, kind) =
      [attr | (attr, bitrep) <- attrBitReps kind, bitrep .&. w /= (toEnum 0)]

{-
  -- This approach doesn't save us anything as we still need to sort the list
  -- (or rely on the implicit order, which is bad).
  where
    collectBits :: [Attr] -> AttrBitRep -> (Array Int Attr) -> [Attr]
    collectBits lst 0 _ = lst
    collectBits lst w arr =
      go (countTrailingZeros w)
      where
        lastBit = finiteBitSize (undefined :: AttrBitRep) - 1
        go :: Int -> [Attr]
        go !ind =
          if ind == lastBit
            then (arr ! lastBit) : lst
            else (arr ! ind) :
                   go (ind + 1 + countTrailingZeros (shiftR w (1 + ind)))
-}

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
  buf' <- safeAdvance buf #{size struct attrreference}
  (off :: Int32) <- #{peek attrreference_t, attr_dataoffset} ptr
  (len :: Word32) <- #{peek attrreference_t, attr_length} ptr
  let iOff = fromEnum off
      iLen = fromEnum len
   in if iOff + iLen > size
        then error "attrreference out of buffer"
        else return (buf', Buf (ptr `plusPtr` iOff) iLen)

-- | Peek a NUL-terminated string referenced by @attreference@ and wrap it
-- into the provided constructor. The terminating NUL character is removed.
peekStringAttr :: (String -> a) -> AttrListBuf -> IO (AttrListBuf, a)
peekStringAttr valueConstr buf =
  peekAttrReference buf >>= \case
    (buf', Buf _ 0) ->
      return (buf', valueConstr "")
    (buf', Buf strPtr strLen) -> do
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
    ATTR_CMN_RETURNED_ATTRS : _ -> do
      (buf', attrSet@(AttributeSet attrs')) <-
        peekAttributeSet True buf
--      putStrLn (show attrs')
--      putStrLn (show [attr | attr <- attrs, not (attr `elem` attrs')])
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
-- Valid args finder.
-------------------------------------------------------------------------------

findValidArgs :: [FsOpt] -> [Attr] -> FilePath -> IO ([Attr], [Attr])
findValidArgs opts0 attrs0 path =
  go opts0 [minBound..maxBound] [] []
  where
  go opts attrs delAttrs tryAttrs =
    tryThis opts (tryAttrs ++ attrs0) >>= \case
      True ->
        case attrs of
          ha : ta ->
            go opts ta delAttrs (ha : tryAttrs)
          _ -> return $ (reverse tryAttrs, reverse delAttrs)
      False ->
        case (attrs, tryAttrs) of
           (ha : ta, ht : tt) ->
             go opts ta (ht : delAttrs) (ha : tt)
           (ha : ta, []) ->
             go opts ta delAttrs [ha]
           ([], ht : tt) ->
             return $ (reverse tt, reverse $ ht : delAttrs)
           _ -> return ([], reverse delAttrs)
  tryThis opts attrs =
    catch
      (do
--        putStrLn ("Trying " ++ (show opts) ++ " " ++ (show attrs))
        _ <- getAttrList opts attrs path
--        putStrLn ("Success " ++ (show opts) ++ " " ++ (show attrs))
        return True)
      (\(e :: IOError) -> do
--         putStrLn ("Failure " ++ (show opts) ++ " " ++ (show attrs))
         case ioe_type e of
           InvalidArgument -> return False
           UnsupportedOperation -> return False
           _ -> throw e)

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
--    putStrLn ("commonattr " ++ (show $ Base2 (getField AttrCmn)))
    #{poke struct attrlist, commonattr} ptr $ getField AttrCmn
--    putStrLn ("volattr " ++ (show $ Base2 ((getField AttrVol) .|. #{const ATTR_VOL_INFO})))
    #{poke struct attrlist, volattr} ptr $ getField AttrVol
--      if (getField AttrVol) /= 0
--        then (getField AttrVol) .|. #{const ATTR_VOL_INFO}
--        else 0
    #{poke struct attrlist, dirattr} ptr $ (0 :: Word32)
    #{poke struct attrlist, fileattr} ptr $ (0 :: Word32)
    #{poke struct attrlist, forkattr} ptr $ (0 :: Word32)
    where
      getField kind =
        foldr (.|.) 0 [bitRep | (k, bitRep, _, _) <- map attrConf attrs,
                                k == kind]
  peek _ = error "do not need yet"

calcBufSize :: [Attr] -> Int
calcBufSize attrs =
  foldr (+) 0 [size | (_, _, size, _) <- map attrConf attrs]

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
getAttrList :: [FsOpt] -> [Attr] -> FilePath -> IO [AttrValue]
getAttrList opts attrs path =
  withFilePath path $ \cPath -> do
    withAttrList (AttrList attrs) $ \pAttrList -> do
      result <- go cPath pAttrList (calcBufSize attrs)
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
            cPath
            pAttrList
            pAttrBuf
            (toEnum bufSize)
            (((packEnumBitFlags (EnumBitFlags opts)) :: CULong) .|.
              #{const FSOPT_REPORT_FULLSIZE}))
        (size :: Int) <-
          (fromIntegral <$> peek ((castPtr pAttrBuf) :: Ptr Word32))
        if size > bufSize
          then
            Left <$> return size
          else
            Right <$> peekAttrs
              (sort attrs)
              (Buf (pAttrBuf `plusPtr` 4) (size - 4))
