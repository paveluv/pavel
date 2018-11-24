{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module Pavel.Xnu.Types
  ( FsId(..)
  , FsObjId(..)
  , CTextEncoding(..)
  , CDarwinTime(..)
  , TimeSpec(..)
  , RawFinderInfo
  , FileFlags
  , FsOpt(..)  -- from CEnums.hs
  , VType(..)  -- from CEnums.hs
  , VTagType(..)  -- from CEnums.hs
  , FileFlag(..)  -- from CEnums.hs
  ) where

-------------------------------------------------------------------------------
-- | Types for interfacing with XNU kernel.
-- |
-- | This module doesn't define types that are already in 'System.Posix.Types'.
-------------------------------------------------------------------------------

import qualified Data.ByteString as BS
import Data.Int
import Data.Word
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import Pavel.Xnu.Types.CEnums
import Pavel.Xnu.EnumBitFlags

#include <sys/attr.h>
#include <sys/_types/_fsid_t.h>
#include <sys/_types/_timespec.h>

-- | File system id type.
-- Corresponds to @struct fsid@ defined in
-- @/usr/include/sys/_types/_fsid_t.h@.
-- Returned by 'getAttrList' for @ATTR_CMN_FSID@.
data FsId =
  FsId !Int32 !Int32
  deriving (Eq, Ord, Read, Show)

instance Storable FsId where
  sizeOf _ = #{size struct fsid}
  alignment _ = #{alignment struct fsid}
  peek ptr = do
    first <- peek (#{ptr struct fsid, val[0]} ptr)
    second <- peek (#{ptr struct fsid, val[1]} ptr)
    return $ FsId first second
  poke ptr (FsId first second) = do
    poke (#{ptr struct fsid, val[0]} ptr) first
    poke (#{ptr struct fsid, val[1]} ptr) second

-- | Structure that uniquely identifies the file system object within
-- a mounted volume for the duration of it's mount.
--
-- Corresponds to @fsobj_id_t@ from @/usr/include/sys/_types/_fsobj_id_t.h@
--
-- According to @getattrlist@ man page @ATTR_CMN_OBJID@ is deprecated in favor
-- of @ATTR_CMNEXT_LINKID@ (sarting with macOS 10.13, iOS 11.0, watchOS 4.0
-- and tvOS 11.0), though it's not clear where to get replacements for
-- @ATTR_CMN_OBJPERMANENTID@ and @ATTR_CMN_PAROBJID@.
data FsObjId = FsObjId
  { fid_objno :: !Word32
  , fid_generation :: !Word32
  } deriving (Eq, Read, Show)

instance Storable FsObjId where
  sizeOf _ = #{size fsobj_id_t}
  alignment _ = #{alignment fsobj_id_t}
  peek ptr = do
    objNo <- #{peek fsobj_id_t, fid_objno} ptr
    generation <- #{peek fsobj_id_t, fid_generation} ptr
    return $ FsObjId { fid_objno = objNo, fid_generation = generation }
  poke ptr fsObjId = do
    #{poke fsobj_id_t, fid_objno} ptr (fid_objno fsObjId)
    #{poke fsobj_id_t, fid_generation} ptr (fid_generation fsObjId)

-- | A text encoding hint for the file system object's name.
-- From @/usr/include/sys/attr.h@.
-- Returned by 'getAttrList' for @ATTR_CMN_SCRIPT@.
newtype CTextEncoding =
  CTextEncoding #{type text_encoding_t}
  deriving (Eq, Ord, Storable)
  deriving newtype (Read, Show)

-- | __darwin_time_t
newtype CDarwinTime =
  CDarwinTime #{type __darwin_time_t}
  deriving (Eq, Ord, Storable)
  deriving newtype (Read, Show)

-- | Corresponds to @struct timespec@ defined in
-- @/usr/include/sys/_types/_timespec.h@.
data TimeSpec = TimeSpec
  { tv_sec :: ! CDarwinTime
  , tv_nsec :: ! #{type long}
  }
  deriving (Eq, Ord, Read, Show)

instance Storable TimeSpec where
  sizeOf _ = #{size _STRUCT_TIMESPEC}
  alignment _ = #{alignment _STRUCT_TIMESPEC}
  peek ptr = do
    sec <- #{peek _STRUCT_TIMESPEC, tv_sec} ptr
    nsec <- #{peek _STRUCT_TIMESPEC, tv_nsec} ptr
    return $ TimeSpec { tv_sec = sec, tv_nsec = nsec }
  poke ptr timespec = do
    #{poke _STRUCT_TIMESPEC, tv_sec} ptr (tv_sec timespec)
    #{poke _STRUCT_TIMESPEC, tv_nsec} ptr (tv_nsec timespec)

-- | 32 bytes of data for use by the Finder.
-- The interpretation depends on the FS object type (file or dir).
-- Returned by 'getAttrList' for @ATTR_CMN_FNDRINFO@.
-- Hmm, it seems to always return 32 zero bytes so far...
newtype RawFinderInfo =
  RawFinderInfo BS.ByteString
  deriving (Read, Show)

instance Storable RawFinderInfo where
  sizeOf _ = 32
  alignment _ = 4
  peek ptr =
    RawFinderInfo <$> BS.packCStringLen ((castPtr ptr), 32)
  poke ptr (RawFinderInfo bs) =
    BS.useAsCStringLen bs $ \(buf, _) -> do
      copyBytes (castPtr ptr) buf 32

-- | File flags are returned in @st_flags@ field by @stat@ and for
-- @ATTR_CMN_FLAGS@ attr by @getAttrList@.
type FileFlags = EnumBitFlags Word32 FileFlag