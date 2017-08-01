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

import           Control.Monad.ST
import qualified Data.ByteString.Lazy      as BS
import           Data.Foldable             (toList)
import           Data.Maybe                (fromJust, mapMaybe)
import           Data.Sequence             hiding (index)
import qualified Data.Sequence             as S
import qualified Data.Vector               as V
import qualified Data.Vector.Mutable       as VM
import           Graphics.Rendering.OpenGL (GLfloat, GLuint, Vector2, Vector3,
                                            Vertex3)

-- | Some Obj's often have more than one mesh, a Group is one such mesh within an Obj.
data Group = Group {
  groupName    :: !(Maybe BS.ByteString),
  vertices     :: !(Seq (Vertex3 GLfloat)),
  textureCoord :: !(Maybe (Seq (Vector2 GLfloat))),
  normals      :: !(Maybe (Seq (Vector3 GLfloat))),
  index        :: !(Seq IndexBlock)
} deriving (Show)

-- | Represents data of an Obj file.
data Obj = Obj {
  objName :: !(Maybe BS.ByteString),
  mtllib  :: Mtllib,
  groups  :: [Group]
} deriving (Show)

-- | Some Obj's formats support materials, that are applied to some faces within a mesh,
-- an IndexBlock represents a set of faces with an applied material.
data IndexBlock = IndexBlock {
  indexBlockame :: Maybe BS.ByteString,
  shading       :: Shading,
  usemtl        :: Usemtl,
  faces         :: Seq Face
} deriving (Show)

data FaceNode = FaceNode {
  nodeIndex         :: GLuint,
  nodeTextureCoords :: Maybe GLuint,
  nodeNormal        :: Maybe GLuint
} deriving (Show)

-- | A face within a mesh, might be composed of more than 3 points.
type Face = Seq FaceNode

type Shading = Maybe Int

type Usemtl = Maybe BS.ByteString

type Mtllib = Maybe BS.ByteString


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
    aux [x]      = []
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

--------------------------------------------------------------------------------
-- Same as above but with Seq.
--------------------------------------------------------------------------------

faceTriangesS :: Face -> S.Seq FaceNode
faceTriangesS f = if S.null f then S.empty else aux $ viewl rest
  where
    (pivot :< rest) = viewl f
    aux (x :< sx) = case viewl sx of
      S.EmptyL -> S.empty
      (y :< _) -> (S.singleton pivot |> x |> y) S.>< aux (viewl sx)

indexBlockTriangesS :: IndexBlock -> S.Seq FaceNode
indexBlockTriangesS = foldl (S.><) S.empty . fmap faceTriangesS . faces

groupTriangesS :: Group -> S.Seq FaceNode
groupTriangesS = foldl (S.><) S.empty . fmap indexBlockTriangesS . index

indexesS :: Group -> S.Seq GLuint
indexesS = fmap nodeIndex . groupTriangesS

textureCoordIndexS :: Group -> S.Seq GLuint
textureCoordIndexS = fmap (fromJust . nodeTextureCoords) . groupTriangesS

normalsIndexS :: Group -> S.Seq GLuint
normalsIndexS = fmap (fromJust . nodeNormal) . groupTriangesS

allIndexesS :: Group -> (S.Seq GLuint,S.Seq GLuint,S.Seq GLuint)
allIndexesS g = (fmap nodeIndex triangles,
    fmap (fromJust . nodeTextureCoords) triangles,
    fmap (fromJust . nodeNormal) triangles)
  where
    triangles = groupTriangesS g

--------------------------------------------------------------------------------
-- Same as above but with vectors.
--------------------------------------------------------------------------------

faceTriangesNum :: Face -> Int
faceTriangesNum = subtract 2 . S.length

indexBlockTriangesNum :: IndexBlock -> Int
indexBlockTriangesNum = sum . fmap faceTriangesNum . faces

groupTriangesNum :: Group -> Int
groupTriangesNum = sum . fmap indexBlockTriangesNum . index

faceTriangesV :: VM.MVector s FaceNode -> Face -> Int -> ST s Int
faceTriangesV arr f i = aux i $ viewl rest
  where
    (pivot :< rest) = viewl f
    aux index (x :< sx) = case viewl sx of
      S.EmptyL -> return index
      (y :< _) -> do
        VM.write arr index pivot
        VM.write arr (index+1) x
        VM.write arr (index+2) y
        aux (index + 3) (viewl sx)

indexBlockTriangesV :: VM.MVector s FaceNode -> IndexBlock -> Int -> ST s Int
indexBlockTriangesV arr ib i = foldr ((=<<) . faceTriangesV arr) (return i) $ faces ib

groupTriangesV :: Group -> V.Vector FaceNode
groupTriangesV g = V.create $ do
  let size = groupTriangesNum g * 3
  arr <- VM.unsafeNew size -- i don't care about initialization.
  foldr ((=<<) . indexBlockTriangesV arr) (return 0) $ index g
  return arr

-- | Returns a list of the vertex indexes as triangles.
indexesV :: Group -> V.Vector GLuint
indexesV = V.map nodeIndex . groupTriangesV

-- | Returns a list of the texture coordinates indexes as triangles.
textureCoordIndexV :: Group -> V.Vector GLuint
textureCoordIndexV = V.map (fromJust . nodeTextureCoords) . groupTriangesV

-- | Returns a list of the normal indexes as triangles.
normalsIndexV :: Group -> V.Vector GLuint
normalsIndexV = V.map (fromJust . nodeNormal) . groupTriangesV

-- | Returns the vertex indexes, texture coordinates indexes and normal indexes of a group.
allIndexesV :: Group -> (V.Vector GLuint,V.Vector GLuint,V.Vector GLuint)
allIndexesV g = (V.map nodeIndex triangles,
    V.map (fromJust . nodeTextureCoords) triangles,
    V.map (fromJust . nodeNormal) triangles)
  where
    triangles = groupTriangesV g
