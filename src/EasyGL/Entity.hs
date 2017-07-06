module EasyGL.Entity(Entity,readObj2Ent,obj2Ent,renderEnt,indexedModel2Ent) where

import qualified EasyGL.Obj.Obj as Obj
import qualified EasyGL.Shader as S
import qualified EasyGL.IndexedModel as IM

import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import qualified Data.Vector.Storable as VS

import Graphics.Rendering.OpenGL
import System.IO
import Control.Monad

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
  --seting vertex attrib pointer to make aware of propiety
  maybe (return ()) setVertexAttribPointer m
  return buffer

makeArrayBufferObject = do
  buffer <- genObjectName :: IO VertexArrayObject
  bindVertexArrayObject $= Just buffer
  return buffer

data SubEntity = SubEntity {
  vertexArrObject :: VertexArrayObject,
  vertexes :: BufferObject, --most allways occur
  textureCoord :: Maybe BufferObject,
  normals :: Maybe BufferObject,
  indexBuffer :: BufferObject,--most allways occur
  vertexNum :: !Int,
  vertexIndexNum :: !Int
} deriving (Show)

type Entity = [SubEntity]

indexedModel2Ent :: [IM.IndexedModel] -> IO Entity
indexedModel2Ent = mapM fromIM

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

readObj2Ent :: String -> IO Entity
readObj2Ent s = (Obj.readObj <$> readFile s) >>= indexedModel2Ent . map Obj.toIndexedModel . Obj.groups

obj2Ent :: Obj.Obj -> IO Entity
obj2Ent = indexedModel2Ent . map Obj.toIndexedModel . Obj.groups

--obj2Ent :: Obj.Obj -> IO Entity
--obj2Ent o = do
--  let k = map Obj.toIndexedModel . Obj.groups $ o
--  print k
--  indexedModel2Ent k

renderEnt :: S.Shader -> Entity -> IO () -> IO ()
renderEnt shader ent shaderAction = S.withShader shader $ do
  shaderAction
  mapM_ renderSubEnt ent

renderSubEnt :: SubEntity -> IO ()
renderSubEnt e = do
  bindVertexArrayObject $= Just (vertexArrObject e)
  --drawElements Triangles (fromIntegral.vertexIndexNum $ e) UnsignedInt (vertexIndex e)
  drawElementsBaseVertex Triangles (fromIntegral.vertexIndexNum $ e) UnsignedInt nullPtr 0
  bindVertexArrayObject $= Nothing





















--
