module Pavel.Xnu.AttrList.Peekery where

import Foreign
import Foreign.C.String

type Buf = CStringLen

advanceBuf :: Buf -> Int -> Buf
advanceBuf (ptr, size) advance = ((ptr `plusPtr` advance), size - advance)

safePeek ::
     forall a. (Storable a)
  => Buf
  -> IO (Buf, a)
safePeek buf@(ptr, size) =
  let advance = (3 + sizeOf (undefined :: a)) .&. (complement 3) -- 4-byte alignment
   in if advance > size
        then ioError $ userError "out of buffer"
        else do
          value <- peek (castPtr ptr)
          return (buf `advanceBuf` advance, value)

peekFixedAttr ::
     forall a b. (Storable a)
  => (a -> b)
  -> Buf
  -> IO (Buf, b)
peekFixedAttr valueConstr buf = do
  (buf', value) <- safePeek buf
  return (buf', valueConstr value)

peekEnumAttr ::
     forall a b c. (Enum a, Integral c, Storable c)
  => c
  -> (a -> b)
  -> Buf
  -> IO (Buf, b)
peekEnumAttr _ valueConstr buf = do
  (buf', value :: c) <- safePeek buf
  return (buf', valueConstr $ toEnum $ fromIntegral value)

peekAttrReference :: Buf -> IO (Buf, Buf)
peekAttrReference buf@(ptr, size) = do
  (buf', off :: Int32) <- safePeek buf
  (buf'', len :: Word32) <- safePeek buf'
  let iOff = (fromIntegral off) :: Int
      iLen = (fromIntegral len) :: Int
   in if iOff + iLen > size
        then ioError $ userError "attrreference out of buffer"
        else return (buf'', ((ptr `plusPtr` iOff), iLen))

peekStringAttr :: (String -> a) -> Buf -> IO (Buf, a)
peekStringAttr valueConstr buf = do
  (buf', strBuf) <- peekAttrReference buf
  str <- peekCStringLen strBuf
  return (buf', valueConstr str)
