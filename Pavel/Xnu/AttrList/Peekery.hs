module Pavel.Xnu.AttrList.Peekery where

-------------------------------------------------------------------------------
-- | Helper peek functions.
-------------------------------------------------------------------------------
import Foreign
import Foreign.C.String

import Pavel.SafeBuf
import Pavel.Xnu.Types

-- | Peek something and wrap it in a constructor.
peekFixedAttr :: (Storable v, SafeBuf b) => (v -> a) -> b -> IO (b, a)
peekFixedAttr valueConstr buf = do
  (buf', value) <- safePeek buf
  return (buf', valueConstr value)

-- | Peek an enum stored as type c (some enums are stored as Word16 and some as
-- Word32). Note, the pointer will be advanced according to buf type @w@ anyway.
peekEnumAttr ::
     forall a b v w. (Enum v, Enum w, Storable w, SafeBuf b)
  => w
  -> (v -> a)
  -> b
  -> IO (b, a)
peekEnumAttr _ valueConstr buf = do
  (buf', value :: w) <- safePeek buf
  return (buf', valueConstr $ toEnum $ fromEnum value)

type AttrListBuf = Buf Word32

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
