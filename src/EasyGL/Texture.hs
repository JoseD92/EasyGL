--------------------------------------------------------------------------------
-- |
-- Module      :  EasyGL.Texture
-- Copyright   :  Copyright (c) 2017, Jose Daniel Duran Toro
-- License     :  BSD3
--
-- Maintainer  :  Jose Daniel Duran Toro <jose_daniel_d@hotmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Helps to load and manage textures in the gpu.
--
--------------------------------------------------------------------------------

module EasyGL.Texture (
  Texture,
  ClampType(..),
  FilteringType(..),
  createTexture2DStatic,
  deleteTexture
)
where
import           Control.Monad.IO.Class
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable
import qualified Graphics.GL            as GL

-- | Textue clamping alternatives.
data ClampType = REPEAT | MIRRORED_REPEAT | CLAMP2EDGE | CLAMP2BORDER
clampType2Number :: ClampType -> GL.GLint
clampType2Number REPEAT          = fromIntegral GL.GL_REPEAT
clampType2Number MIRRORED_REPEAT = fromIntegral GL.GL_MIRRORED_REPEAT
clampType2Number CLAMP2EDGE      = fromIntegral GL.GL_CLAMP_TO_EDGE
clampType2Number CLAMP2BORDER    = fromIntegral GL.GL_CLAMP_TO_BORDER

-- | Texture filtering alternatives.
data FilteringType = NEAREST | LINEAR
filteringType2Number :: FilteringType -> GL.GLint
filteringType2Number NEAREST = fromIntegral GL.GL_NEAREST
filteringType2Number LINEAR  = fromIntegral GL.GL_LINEAR

type Texture = GL.GLuint

-- | Creates and loads to gpu a texture.
createTexture2DStatic :: (Integral a,MonadIO m) => a -> a -> Ptr () -> ClampType -> FilteringType -> m Texture
createTexture2DStatic width height dataPtr ct ft = liftIO $ with 0 $ \name -> do
  GL.glGenTextures 1 name
  val <- peek name
  GL.glBindTexture GL.GL_TEXTURE_2D val
  GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_WRAP_S $ clampType2Number ct
  GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_WRAP_T $ clampType2Number ct
  GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_MIN_FILTER $ filteringType2Number ft
  GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_MAG_FILTER $ filteringType2Number ft
  GL.glTexImage2D GL.GL_TEXTURE_2D 0 (fromIntegral GL.GL_RGBA) (fromIntegral width) (fromIntegral height) 0 GL.GL_RGBA GL.GL_UNSIGNED_BYTE dataPtr
  GL.glGenerateMipmap GL.GL_TEXTURE_2D
  return val

-- | Deletes from gpu a texture.
deleteTexture :: (MonadIO m) => Texture -> m ()
deleteTexture val = liftIO $ with val $ \ptr -> GL.glDeleteTextures 1 ptr
