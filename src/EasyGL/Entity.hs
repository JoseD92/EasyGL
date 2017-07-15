--------------------------------------------------------------------------------
-- |
-- Module      :  EasyGL.Entity
-- Copyright   :  Copyright (c) 2017, Jose Daniel Duran Toro
-- License     :  BSD3
--
-- Maintainer  :  Jose Daniel Duran Toro <jose_daniel_d@hotmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Assists in the process of loading a mesh to gpu and drawing it with OpenGL.
--
--------------------------------------------------------------------------------

module EasyGL.Entity(
  Entity,
  readObj2Ent,
  obj2Ent,
  renderEnt,
  indexedModel2Ent,
  deleteEntity
  ) where

import qualified EasyGL.IndexedModel       as IM
import qualified EasyGL.Obj                as Obj
import qualified EasyGL.Shader             as S

import qualified Data.Vector.Storable      as VS
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable

import           Control.Monad
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Graphics.Rendering.OpenGL
import           System.IO

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

setVertexAttribPointer :: (GLuint,NumComponents) -> IO ()
setVertexAttribPointer (location,numComponents) = do
  vertexAttribArray (AttribLocation location) $= Enabled
  vertexAttribPointer (AttribLocation location) $=
    (ToFloat, VertexArrayDescriptor numComponents Float 0 (bufferOffset 0))

cleanVertexAttribPointer :: (GLuint,NumComponents) -> IO ()
cleanVertexAttribPointer (location,_) = vertexAttribArray (AttribLocation location) $= Disabled

makeBufferObject :: (Storable a) => BufferTarget -> VS.Vector a -> BufferUsage -> Maybe (GLuint,NumComponents) -> IO BufferObject
makeBufferObject target elems usage m = do
  --creating buffer
  buffer <- genObjectName
  bindBuffer target $= Just buffer
  --filling buffer
  VS.unsafeWith elems $ \ptr -> do
    let size = fromIntegral (VS.length elems * sizeOf (VS.head elems))
    bufferData target $= (size, ptr, usage)
  --setting vertex attrib pointer to be aware of buffer
  maybe (return ()) setVertexAttribPointer m
  return buffer

makeArrayBufferObject = do
  buffer <- genObjectName :: IO VertexArrayObject
  bindVertexArrayObject $= Just buffer
  return buffer

data SubEntity = SubEntity {
  vertexArrObject :: VertexArrayObject,
  vertexes        :: BufferObject, --most always occur
  textureCoord    :: Maybe BufferObject,
  normals         :: Maybe BufferObject,
  indexBuffer     :: BufferObject,--most always occur
  vertexNum       :: !Int,
  vertexIndexNum  :: !Int
} deriving (Show)

-- | Represents a mesh with possibly normals and texture coordinates, that is loaded in gpu and can be draw within an OpenGL environment.
type Entity = [SubEntity]

-- | Given an indexed model, generates an entity.
indexedModel2Ent :: MonadIO m => [IM.IndexedModel] -> m Entity
indexedModel2Ent = mapM (liftIO . fromIM)

fromIM :: IM.IndexedModel -> IO SubEntity
fromIM g = do
  mesh <- makeArrayBufferObject
  vertexBuffer <- makeBufferObject ArrayBuffer (IM.vertices g) StaticDraw $ Just (0,3)
  textureCoord <- if VS.null (IM.textureCoord g) then return Nothing else
    fmap Just $ makeBufferObject ArrayBuffer (IM.textureCoord g) StaticDraw $ Just (1,2)
  normalBuffer <- if VS.null (IM.normals g) then return Nothing else
    fmap Just $ makeBufferObject ArrayBuffer (IM.normals g) StaticDraw $ Just (2,3)
  indexBuffer <- makeBufferObject ElementArrayBuffer (IM.indexes g) StaticDraw Nothing
  bindVertexArrayObject $= Nothing
  return (SubEntity mesh vertexBuffer Nothing normalBuffer indexBuffer (VS.length (IM.vertices g)) (VS.length (IM.indexes g)))

-- | Given the .obj text, generates an entity.
readObj2Ent :: MonadIO m => String -> m Entity
readObj2Ent s = liftIO $ (Obj.readObj <$> readFile s) >>= indexedModel2Ent . map Obj.toIndexedModel . Obj.groups

-- | Given an .obj mesh, generates an entity.
obj2Ent :: MonadIO m => Obj.Obj -> m Entity
obj2Ent = indexedModel2Ent . map Obj.toIndexedModel . Obj.groups

-- | Given a shader and an io action (that should only set the shader uniform variables), draws with OpenGL the given Entity.
renderEnt :: MonadIO m => S.Shader -> Entity -> S.Uniform () -> m ()
renderEnt shader ent uniformAction = S.withShaderSafe shader uniformAction $ liftIO $ mapM_ renderSubEnt ent

renderSubEnt :: SubEntity -> IO ()
renderSubEnt e = do
  bindVertexArrayObject $= Just (vertexArrObject e)
  --drawElements Triangles (fromIntegral.vertexIndexNum $ e) UnsignedInt (vertexIndex e)
  drawElementsBaseVertex Triangles (fromIntegral.vertexIndexNum $ e) UnsignedInt nullPtr 0
  bindVertexArrayObject $= Nothing

-- | Deletes and entity and frees gpu memory.
deleteEntity :: MonadIO m => Entity -> m ()
deleteEntity = mapM_ deleteSubEntity

deleteSubEntity :: MonadIO m => SubEntity -> m ()
deleteSubEntity se = do
  deleteObjectName $ vertexArrObject se
  deleteObjectName $ vertexes se
  deleteObjectName $ indexBuffer se
  forM_ (textureCoord se) deleteObjectName
  forM_ (normals se) deleteObjectName



















--
