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
  EasyGL.Obj.ObjData.FaceNode(..),
  EasyGL.Obj.ObjData.Face(..),
  EasyGL.Obj.ObjData.Shading(..),
  EasyGL.Obj.ObjData.Usemtl(..),
  EasyGL.Obj.ObjData.Mtllib(..),
  EasyGL.Obj.ObjData.IndexBlock(..),
  readObj,
  readObj2Ent,
  obj2Ent,
  toIndexedModel,
  groupToIndexedModel,

  faceTrianges,
  indexBlockTrianges,
  groupTrianges,
  indexes,
  textureCoordIndex,
  normalsIndex,
  allIndexes,

  faceTriangesS,
  indexBlockTriangesS,
  groupTriangesS,
  indexesS,
  textureCoordIndexS,
  normalsIndexS,
  allIndexesS,

  groupTriangesV,
  indexesV,
  textureCoordIndexV,
  normalsIndexV,
  allIndexesV
  )
where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Lazy   as BS
import           EasyGL.Entity
import           EasyGL.Obj.Grammar
import           EasyGL.Obj.Obj2IM
import           EasyGL.Obj.ObjData
import           EasyGL.Obj.Tokens

-- | Parse a String into an Obj.
readObj :: BS.ByteString -> Obj
readObj = parseObj.alexScanTokens

-- | Given the .obj text, generates an entity.
readObj2Ent :: MonadIO m => String -> m Entity
readObj2Ent s = liftIO $ (readObj <$> BS.readFile s) >>= indexedModel2Ent . toIndexedModel

-- | Given an .obj mesh, generates an entity.
obj2Ent :: MonadIO m => Obj -> m Entity
obj2Ent = indexedModel2Ent . toIndexedModel
