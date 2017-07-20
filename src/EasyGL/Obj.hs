--------------------------------------------------------------------------------
-- |
-- Module      :  EasyGL.Obj
-- Copyright   :  Copyright (c) 2017, Jose Daniel Duran Toro
-- License     :  BSD3
--
-- Maintainer  :  Jose Daniel Duran Toro <jose_daniel_d@hotmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Loading and Parsing of Obj files.
--
--------------------------------------------------------------------------------

module EasyGL.Obj (
  EasyGL.Obj.ObjData.Obj(..),
  EasyGL.Obj.ObjData.Group(..),
  readObj,
  toIndexedModel
  )
where

import EasyGL.Obj.Grammar
import EasyGL.Obj.Tokens
import EasyGL.Obj.ObjData
import EasyGL.Obj.Obj2IM

-- | Parse a String into an Obj.
readObj :: String -> Obj
readObj = parseObj.alexScanTokens
