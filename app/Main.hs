module Main where

import Control.Monad
import Pavel.Xnu.AttrList
import Pavel.Xnu.EnumBitFlags
import Pavel.Xnu.Types
import System.Environment

main :: IO ()
main =
  let opts1 = EnumBitFlags [FSOPT_NOFOLLOW, FSOPT_ATTR_CMN_EXTENDED]
      opts2 = EnumBitFlags [FSOPT_NOFOLLOW]
   in do files <- getArgs
         forM_ files $ \file -> do
           st <-
             getAttrList
               opts1
               AttrList
                 { commonAttr
                       -- ATTR_CMN_NAME
                    =
                     [ ATTR_CMN_DEVID
                     , ATTR_CMN_FSID
                     , ATTR_CMN_OBJTYPE
                     , ATTR_CMN_OBJTAG
  --                   , ATTR_CMN_OBJID
  --                   , ATTR_CMN_OBJPERMANENTID
  --                   , ATTR_CMN_PAROBJID
  --                   , ATTR_CMN_SCRIPT
                     , ATTR_CMN_CRTIME
                     , ATTR_CMN_MODTIME
                     , ATTR_CMN_CHGTIME
                     , ATTR_CMN_ACCTIME
                     , ATTR_CMN_BKUPTIME
  --                   , ATTR_CMN_FNDRINFO
                     , ATTR_CMN_GRPID
                     , ATTR_CMN_OWNERID
                     , ATTR_CMN_ACCESSMASK
                     , ATTR_CMN_FLAGS
                     , ATTR_CMN_GEN_COUNT
                     , ATTR_CMN_DOCUMENT_ID
                     ]
                 }
               file
           putStrLn (show $ st)
