{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Control.Monad
import qualified Data.Text as T
import Pavel.Xnu.AttrList
import Pavel.Xnu.Types
import System.Environment

main :: IO ()
main =
  let _opts1 =
         [FSOPT_NOFOLLOW, FSOPT_ATTR_CMN_EXTENDED, FSOPT_PACK_INVAL_ATTRS]
      _opts2 = [FSOPT_ATTR_CMN_EXTENDED, FSOPT_PACK_INVAL_ATTRS]
   in do (attrStr:files) <- getArgs
         let (attrs :: [Attr]) =
               map
                 read
                 [ (T.unpack s)
                 | s <- (T.split (== ',') (T.pack attrStr))
                 , not (T.isPrefixOf "_" s)
                 ]
          in forM_ files $ \file -> do
               putStrLn ("attrs " <> (show attrs))
               (valid, invalid) <-
                 findValidArgs
                   _opts2
                   attrs
                   file
               putStrLn ("Valid attrs " ++ (show $ valid))
               putStrLn ("Invalid attrs " ++ (show $ invalid))
               values <-
                 getAttrList _opts2 valid file
               putStrLn (show values)
