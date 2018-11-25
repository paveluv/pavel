{-# LANGUAGE InstanceSigs #-}

module Pavel.SafeBuf where

import Control.Monad
import Foreign

class SafeBuf b where
  -- | Safely advance by number of bytes.
  safeAdvance :: b -> Int -> IO b
  -- | Safely peek a storable object and throw an error if the peek failed.
  -- Returns another buffer for subsequent consecutive safe peeks.
  safePeek :: (Storable a) => b -> IO (b, a)
  -- | Safe peek a consecutive list of storable objects.
  safePeekList :: (Storable a) => b -> Int -> IO (b, [a])
  safePeekList buf count =
    foldM
      (\(buf', lst) _ -> (\(buf'', val) -> (buf'', val : lst)) <$> safePeek buf')
      (buf, [])
      [1 .. count]

-- | Just a pointer to @w@, and size in bytes.
-- @safePeek@ and @safeAdvance@ operations on it will advance the buffer
-- in multiples of @(sizeOf (undefined :: w))@ bytes and throw on
-- end of buffer crossing.
data Buf w =
  Buf (Ptr w)
      Int

-- | Throws if the advance goes beyond the end of buffer.
instance (Storable w) => SafeBuf (Buf w) where
  safeAdvance (Buf ptr size) advance =
    let align = sizeOf (undefined :: w)
        ptr' = alignPtr (ptr `plusPtr` advance) align
        size' = size - (ptr' `minusPtr` ptr)
     in if size' < 0
          then error "out of buffer"
          else return $ Buf ptr' size'
  safePeek ::
       forall a. (Storable a)
    => Buf w
    -> IO (Buf w, a)
  safePeek buf@(Buf ptr _) = do
    buf' <- safeAdvance buf (sizeOf (undefined :: a))
    value <- peek (castPtr ptr)
    return (buf', value)
