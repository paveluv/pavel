module Pavel.EnumBitFlags where

import Data.Bits
import Foreign.Ptr
import Foreign.Storable

-- | Helper type for the common case of bitmask flags defined as enum.
--
-- Example usage. Consider a C enum
--
-- > enum someenum {
-- >   FLAG1 = 0x0001,
-- >   FLAG2 = 0x0002,
-- >   ...
-- >   FLAG9 = 0x0100,
-- >   ...
-- > }
--
-- Generate Haskell enum using c2hs
--
-- > {#enum someenum as SomeEnum {} deriving (Bounded,Eq,Ord,Read,Show)#}
--
-- Define a new type
--
-- > type SomeEnumFlags = EnumBitFlags Word32 SomeEnum
newtype EnumBitFlags w a =
  EnumBitFlags [a]
  deriving (Read, Show)

instance Foldable (EnumBitFlags w) where
  foldr func acc (EnumBitFlags flags) = foldr func acc flags

-- | Fold the list of flags into a bitmask word.
foldEnumBitFlags :: (Enum a, Enum w, Bits w) => EnumBitFlags w a -> w
foldEnumBitFlags (EnumBitFlags flags) =
  foldr ((.|.) . toEnum . fromEnum) (toEnum 0) flags

-- | Given a bitmask word, generate a list of flags.
unfoldEnumBitFlags ::
     (Bounded a, Enum a, Enum w, Bits w) => w -> EnumBitFlags w a
unfoldEnumBitFlags word =
  EnumBitFlags
    [e | e <- [minBound ..], (toEnum $ fromEnum e) .&. word /= (toEnum 0)]

-- | A list of flags is storable as a bitmask word.
instance (Bounded a, Enum a, Enum w, Bits w, Storable w) =>
         Storable (EnumBitFlags w a) where
  sizeOf _ = sizeOf (undefined :: w)
  alignment _ = alignment (undefined :: w)
  peek ptr = do
    unfoldEnumBitFlags <$> peek (castPtr ptr :: Ptr w)
  poke ptr = poke (castPtr ptr :: Ptr w) . foldEnumBitFlags
