--------------------------------------------------------------------------------
-- |
-- Module      :  EasyGL.Material
-- Copyright   :  Copyright (c) 2017, Jose Daniel Duran Toro
-- License     :  BSD3
--
-- Maintainer  :  Jose Daniel Duran Toro <jose_daniel_d@hotmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Material allows loading textures to gpu and easily using then within a shader, uniform variable must be set with the uniform monad.
--
--------------------------------------------------------------------------------

module EasyGL.Material(
makeMaterial,
drawWithMat,
deleteMaterial,
Material
) where

import           Codec.Picture          (Image (..), Pixel, PixelBaseComponent,
                                         PixelRGBA8, convertRGBA8, readImage)
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Either
import qualified Data.Map               as Map
import           Data.Vector.Storable   (unsafeWith)
import           Data.Word
import qualified EasyGL.Entity          as Ent
import qualified EasyGL.Shader          as S
import qualified EasyGL.Texture         as T
import           Foreign.Marshal.Array
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable
import qualified Graphics.GL            as GL
import           System.IO

texturesEnums = [GL.GL_TEXTURE0,GL.GL_TEXTURE1,GL.GL_TEXTURE2,GL.GL_TEXTURE3,GL.GL_TEXTURE4,GL.GL_TEXTURE5,GL.GL_TEXTURE6,GL.GL_TEXTURE7,GL.GL_TEXTURE8,GL.GL_TEXTURE9,GL.GL_TEXTURE10,GL.GL_TEXTURE11,GL.GL_TEXTURE12,GL.GL_TEXTURE13,GL.GL_TEXTURE14,GL.GL_TEXTURE15,GL.GL_TEXTURE16,GL.GL_TEXTURE17,GL.GL_TEXTURE18,GL.GL_TEXTURE19,GL.GL_TEXTURE20,GL.GL_TEXTURE21,GL.GL_TEXTURE22,GL.GL_TEXTURE23,GL.GL_TEXTURE24,GL.GL_TEXTURE25,GL.GL_TEXTURE26,GL.GL_TEXTURE27,GL.GL_TEXTURE28,GL.GL_TEXTURE29,GL.GL_TEXTURE30,GL.GL_TEXTURE31]

-- | Data for keeping materials
data Material = Material {
  shader   :: S.Shader,
  textures :: [(T.Texture,String,Word32)]
}

allocTexture :: Image PixelRGBA8 -> IO GL.GLuint
allocTexture image = unsafeWith (imageData image) $ \ptr ->
    T.createTexture2DStatic (fromIntegral . imageWidth $ image) (fromIntegral . imageHeight $ image)
      (castPtr ptr) T.REPEAT T.NEAREST

-- | Creates a material.
makeMaterial :: (MonadIO m) => S.Shader -- ^ Shader for the material.
  -> [(FilePath,String)] -- ^ List of Path to and name of uniform sampler2D for each texture.
  -> m (Either String Material)
makeMaterial shader textures = do
  imgDynamic <- mapM (liftIO . readImage . fst) textures
  if null (lefts imgDynamic) then do
    let images = map convertRGBA8 $ rights imgDynamic
    textureNames <- mapM (liftIO . allocTexture) images
    return . Right $ Material shader $ zip3 textureNames textureUniforms [0..]
  else return $ Left $ unlines $ lefts imgDynamic
  where
    textureUniforms = map snd textures

setTexture :: (GL.GLuint,String,Word32) -> IO (S.Uniform ())
setTexture (name,uniform,number) = do
  GL.glActiveTexture number
  GL.glBindTexture GL.GL_TEXTURE_2D name
  return $ S.set uniform number

-- | Draws an entity using a material.
drawWithMat :: (MonadIO m) => Material -> Ent.Entity -> S.Uniform () -> m ()
drawWithMat m e userUniforms = do
  uni <- liftIO $ mapM setTexture $ textures m
  Ent.renderEnt (shader m) e $ do
    sequence_ uni
    userUniforms

-- | Deletes a material. It only frees the textures from gpu memory, the shader is not free as it may be shared with other materials.
deleteMaterial :: (MonadIO m) => Material -> m ()
deleteMaterial m = mapM_ (\(tex,_,_) -> T.deleteTexture tex) $ textures m
