module EasyGL.Material(
makeMaterial,
drawWithMat,
Material
) where

import qualified EasyGL.Shader as S
import qualified EasyGL.Entity as Ent

import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Data.Vector.Storable (unsafeWith)

import System.IO
import Control.Monad
import Data.Either
import qualified Data.Map as Map

import qualified Graphics.GL as GL
import Data.Word

import Codec.Picture (readImage,convertRGBA8,Image(..),Pixel,PixelRGBA8,PixelBaseComponent)

texturesEnums = [GL.GL_TEXTURE0,GL.GL_TEXTURE1,GL.GL_TEXTURE2,GL.GL_TEXTURE3,GL.GL_TEXTURE4,GL.GL_TEXTURE5,GL.GL_TEXTURE6,GL.GL_TEXTURE7,GL.GL_TEXTURE8,GL.GL_TEXTURE9,GL.GL_TEXTURE10,GL.GL_TEXTURE11,GL.GL_TEXTURE12,GL.GL_TEXTURE13,GL.GL_TEXTURE14,GL.GL_TEXTURE15,GL.GL_TEXTURE16,GL.GL_TEXTURE17,GL.GL_TEXTURE18,GL.GL_TEXTURE19,GL.GL_TEXTURE20,GL.GL_TEXTURE21,GL.GL_TEXTURE22,GL.GL_TEXTURE23,GL.GL_TEXTURE24,GL.GL_TEXTURE25,GL.GL_TEXTURE26,GL.GL_TEXTURE27,GL.GL_TEXTURE28,GL.GL_TEXTURE29,GL.GL_TEXTURE30,GL.GL_TEXTURE31]

data Material = Material {
  name :: String,
  shader :: S.Shader,
  textures :: [(GL.GLuint,String,Word32)]
}

allocTexture :: Image PixelRGBA8 -> IO GL.GLuint
allocTexture image = with 0 $ \name -> do
  GL.glGenTextures 1 name
  val <- peek name
  GL.glBindTexture GL.GL_TEXTURE_2D val
  GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_WRAP_S $ fromIntegral GL.GL_CLAMP_TO_BORDER--GL.GL_REPEAT
  GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_WRAP_T $ fromIntegral GL.GL_CLAMP_TO_BORDER--GL.GL_REPEAT
  GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_MIN_FILTER $ fromIntegral GL.GL_LINEAR
  GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_MAG_FILTER $ fromIntegral GL.GL_LINEAR
  unsafeWith (imageData image) $ \ptr ->
    GL.glTexImage2D GL.GL_TEXTURE_2D 0 (fromIntegral GL.GL_RGBA) (fromIntegral . imageWidth $ image) (fromIntegral . imageHeight $ image) 0 GL.GL_RGBA GL.GL_UNSIGNED_BYTE ptr

  GL.glGenerateMipmap GL.GL_TEXTURE_2D
  return val

makeMaterial :: String -> S.Shader -> [(FilePath,String)] -> IO (Either String Material)
makeMaterial s shader textures = do
  --get maxTextureUnit
  imgDynamic <- mapM (readImage.fst) textures
  if null (lefts imgDynamic) then do
    let images = map convertRGBA8 $ rights imgDynamic
    textureNames <- mapM allocTexture images
    return . Right $ Material s shader $ zip3 textureNames textureUniforms [0..]
  else return $ Left $ unlines $ lefts imgDynamic
  where
    textureUniforms = map snd textures

setTexture :: (GL.GLuint,String,Word32) -> IO ()
setTexture (name,uniform,number) = do
  GL.glActiveTexture number
  GL.glBindTexture GL.GL_TEXTURE_2D name

  S.setVar number uniform

drawWithMat :: Material -> Ent.Entity -> IO () -> IO ()
drawWithMat m e io = Ent.renderEnt (shader m) e $ do
  mapM_ setTexture $ textures m
  io
