--------------------------------------------------------------------------------
-- |
-- Module      :  EasyGL.Obj.Obj2IM
-- Copyright   :  Copyright (c) 2017, Jose Daniel Duran Toro
-- License     :  BSD3
--
-- Maintainer  :  Jose Daniel Duran Toro <jose_daniel_d@hotmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Functions required to transform a Obj to an EasyGL Indexed Model.
--
--------------------------------------------------------------------------------

module EasyGL.Obj.Obj2IM (
toIndexedModel
  )
where
import EasyGL.Obj.ObjData
import Graphics.Rendering.OpenGL
import Data.Array
import qualified EasyGL.IndexedModel as IM
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import Data.Foldable (toList)
import qualified Data.Vector.Storable as VS

toVector :: (VS.Storable a) => [a] -> VS.Vector a
toVector = VS.fromList

-- | Transforms a Obj Group into an indexed model.
toIndexedModel :: Group -> IM.IndexedModel
toIndexedModel g = IM.IndexedModel (toVector vert2) (toVector norm2) (toVector $ map invertY tex1) (toVector index2)
  where
    vert0 = vertices g
    norm0 = normals g
    tex0 = textureCoord g
    index0 = indexes g
    (vert1,norm1,index1) = if null norm0 then (vert0,[],index0) else addNorms vert0 index0 norm0 (normalsIndex g)
    (vert2,norm2,tex1,index2) = if null tex0 then (vert1,norm1,[],index1) else addTexCoords vert1 index1 norm1 tex0 (textureCoordIndex g)
    invertY (Vector2 x y) = Vector2 x (1-y) -- either my image loading library is lying to me, or i actually have to make this transformation, but it works!!!

addNorms :: [Vertex3 GLfloat] -> [GLuint] -> [Vector3 GLfloat] -> [GLuint] -> ([Vertex3 GLfloat],[Vector3 GLfloat],[GLuint])
addNorms verts vertsIndex norms normsIndex = rearrange 0 indexLen vertsLen vertsLookup vertsIndexLookup normsLookup normsIndexLookup resul Seq.empty Seq.empty Map.empty (Vector3 0 0 0)
  where
    vertsLen = fromIntegral $ length verts
    normsLen = fromIntegral $ length norms
    indexLen = fromIntegral $ length vertsIndex -- vertsIndex and normsIndex have the same length
    vertsLookup = listArray (0,vertsLen-1) verts
    vertsIndexLookup = listArray (0,indexLen-1) vertsIndex
    normsLookup = listArray (0,normsLen-1) norms
    normsIndexLookup = listArray (0,indexLen-1) normsIndex
    resul = listArray (0,vertsLen-1) $ repeat Nothing

addTexCoords :: [Vertex3 GLfloat] -> [GLuint] -> [Vector3 GLfloat] -> [Vector2 GLfloat] -> [GLuint] -> ([Vertex3 GLfloat],[Vector3 GLfloat],[Vector2 GLfloat],[GLuint])
addTexCoords verts vertsIndex norms texCood texCoodIndex = (newVerts,newNorm,newtexCood,newIndex)
  where
    vertsLen = fromIntegral $ length verts
    texCoodLen = fromIntegral $ length texCood
    indexLen = fromIntegral $ length vertsIndex -- vertsIndex and normsIndex have the same length
    dataLookup = listArray (0,vertsLen-1) $ zip verts norms
    dataIndexLookup = listArray (0,indexLen-1) vertsIndex
    texCoodLookup = listArray (0,texCoodLen-1) texCood
    texCoodIndexLookup = listArray (0,indexLen-1) texCoodIndex
    resul = listArray (0,vertsLen-1) $ repeat Nothing
    (newData,newtexCood,newIndex) = rearrange 0 indexLen vertsLen dataLookup dataIndexLookup texCoodLookup texCoodIndexLookup resul Seq.empty Seq.empty Map.empty (Vector2 0 0)
    (newVerts,newNorm) = unzip newData

rearrange :: (Ord a,Ord b) => GLuint -> GLuint -> GLuint -> Array GLuint a -> Array GLuint GLuint -> Array GLuint b -> Array GLuint GLuint -> Array GLuint (Maybe b) -> Seq.Seq a -> Seq.Seq b -> Map.Map (a,b) GLuint -> b -> ([a],[b],[GLuint])
rearrange current limit next dataLookup dataIndexLookup newDataLookup newDataIndexLookup resul extraA extraB extraMap elemB
  | current == limit = (elems dataLookup ++ toList extraA,getMaybeArr resul ++ toList extraB,elems dataIndexLookup)
  | otherwise = rearrange (current+1) limit new_Next dataLookup new_DataIndexLookup newDataLookup newDataIndexLookup new_Resul new_extraA new_extraB new_extraMap elemB
  where
    getMaybeArr = map (maybe elemB id) . elems
    currentData = dataLookup ! (dataIndexLookup ! current)
    currentNewData = newDataLookup ! (newDataIndexLookup ! current)
    currentResul = resul ! (dataIndexLookup ! current)  -- its a maybe
    isItInAnyExtra = Map.lookup (currentData,currentNewData) extraMap -- its a maybe
    new_Next = maybe next (const $ maybe (next+1) (const next) isItInAnyExtra) currentResul
    new_DataIndexLookup = maybe dataIndexLookup (const $ maybe (dataIndexLookup // [(current,next)]) (\i -> dataIndexLookup // [(current,i)]) isItInAnyExtra) currentResul
    new_Resul = maybe (resul // [(dataIndexLookup ! current,Just currentNewData)]) (const resul) currentResul
    new_extraA = maybe extraA (const $ maybe (extraA Seq.|> currentData) (const extraA) isItInAnyExtra) currentResul
    new_extraB = maybe extraB (const $ maybe (extraB Seq.|> currentNewData) (const extraB) isItInAnyExtra) currentResul
    new_extraMap = maybe extraMap (const $ maybe (Map.insert (currentData,currentNewData) next extraMap) (const extraMap) isItInAnyExtra) currentResul
