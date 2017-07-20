--------------------------------------------------------------------------------
-- |
-- Module      :  EasyGL.Obj.ObjData
-- Copyright   :  Copyright (c) 2017, Jose Daniel Duran Toro
-- License     :  BSD3
--
-- Maintainer  :  Jose Daniel Duran Toro <jose_daniel_d@hotmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Data types for storing a parsed Obj file.
--
--------------------------------------------------------------------------------

module EasyGL.Obj.ObjData (
  Obj(..),
  Group(..),
  FaceNode(..),
  Face(..),
  Shading(..),
  Usemtl(..),
  Mtllib(..),
  IndexBlock(..),
  faceTrianges,
  indexBlockTrianges,
  groupTrianges,
  indexes,
  textureCoordIndex,
  normalsIndex,
  allIndexes
)
where

import Graphics.Rendering.OpenGL (GLfloat, GLuint, Vertex3, Vector3, Vector2)
import Data.Sequence hiding (index)
import qualified Data.Sequence as S
import Data.Foldable (toList)
import Data.Maybe (mapMaybe)

data Group = Group {
  groupName :: !(Maybe String),
  vertices :: !(Seq (Vertex3 GLfloat)),
  textureCoord :: !(Maybe (Seq (Vector2 GLfloat))),
  normals :: !(Maybe (Seq (Vector3 GLfloat))),
  index :: !(Seq IndexBlock)
} deriving (Show)

-- | Represents data of an Obj file.
data Obj = Obj {
  objName :: !(Maybe String),
  mtllib :: Mtllib,
  groups :: [Group]
} deriving (Show)

data IndexBlock = IndexBlock {
  indexBlockame :: Maybe String,
  shading :: Shading,
  usemtl :: Usemtl,
  faces :: Seq Face
} deriving (Show)

data FaceNode = FaceNode {
  nodeIndex :: GLuint,
  nodeTextureCoords :: Maybe GLuint,
  nodeNormal :: Maybe GLuint
} deriving (Show)

type Face = Seq FaceNode

type Shading = Maybe Int

type Usemtl = Maybe String

type Mtllib = Maybe String


--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

-- | Obj faces can contain more that 3 index, but for a face to be use in computer graphics,
-- it needs to be in the form of triangles, this function turns faces into triangles,
-- every 3 elements of the returned list will be a triangle.
faceTrianges :: Face -> [FaceNode]
faceTrianges f = if S.null f then [] else aux asList
  where
    (pivot :< rest) = viewl f
    asList = toList rest
    aux [x] = []
    aux (x:y:xs) = pivot:x:y:aux (y:xs)

indexBlockTrianges :: IndexBlock -> [FaceNode]
indexBlockTrianges = concatMap faceTrianges . faces

groupTrianges :: Group -> [FaceNode]
groupTrianges = concatMap indexBlockTrianges . index

-- | Returns a list of the vertex indexes as triangles.
indexes :: Group -> [GLuint]
indexes = map nodeIndex . groupTrianges

-- | Returns a list of the texture coordinates indexes as triangles.
textureCoordIndex :: Group -> [GLuint]
textureCoordIndex = mapMaybe nodeTextureCoords . groupTrianges

-- | Returns a list of the normal indexes as triangles.
normalsIndex :: Group -> [GLuint]
normalsIndex = mapMaybe nodeNormal . groupTrianges

-- | Returns the vertex indexes, texture coordinates indexes and normal indexes of a group.
allIndexes :: Group -> ([GLuint],[GLuint],[GLuint])
allIndexes g = (map nodeIndex triangles,
    mapMaybe nodeTextureCoords triangles,
    mapMaybe nodeNormal triangles)
  where
    triangles = groupTrianges g
