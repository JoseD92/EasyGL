--------------------------------------------------------------------------------
-- |
-- Module      :  EasyGL.EasyMem
-- Copyright   :  Copyright (c) 2017, Jose Daniel Duran Toro
-- License     :  BSD3
--
-- Maintainer  :  Jose Daniel Duran Toro <jose_daniel_d@hotmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Helps to manage memory easier.
-- Sometimes people do not remember to run the destructor for a given data type, leaving date in gpu memory, this module provides a way to make the garbage collector clean gpu memory when the object is no longer reachable.
-- Every function in this module behaves the same as the ones in the modules they originally come from, with the difference that objects create from function from this module will call their destructor function on becoming unreachable.
-- Destructor function will only be call when haskell GC runs, witch may never happen, it is recommended to run performMinorGC from module System.Mem, witch will trigger a minor GC, this will ensure that destructors are run for every unreachable object.
-- A GC is expensive, be sure not to run one every frame as you may encounter performance issues, instead run it every so may seconds or every time you think garbage have been generated, like loading a new scene and forgetting the previews one.
-- If you create objects with this function make sure not to call their destructor, I do not know what OpenGL may do on double call for an object destruction, also OpenGL may reuse the name of a destroyed object and erase an object it should not.
-- The only way to ensure correct resource management will be to create an OpenGL monad that manages all of that internally, it may be create in a future update.
--
--------------------------------------------------------------------------------

module EasyGL.EasyMem (
  readObj2Ent,
  obj2Ent,
  indexedModel2Ent,
  loadShadersFromFile,
  loadShadersFromBS,
  makeMaterial,
  createTexture2DStatic
) where

import           Control.Monad
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import qualified Data.ByteString           as BS
import qualified EasyGL.Entity             as E
import qualified EasyGL.IndexedModel       as IM
import qualified EasyGL.Material           as M
import qualified EasyGL.Obj.Obj            as Obj
import qualified EasyGL.Shader             as S
import qualified EasyGL.Texture            as T
import           Foreign.Ptr
import qualified Graphics.Rendering.OpenGL as GL
import           System.IO                 (Handle)
import           System.Mem.Weak

readObj2Ent :: MonadIO m => String -> m E.Entity
readObj2Ent s = do
  e <- E.readObj2Ent s
  liftIO $ mkWeakPtr e $ Just $ E.deleteEntity e
  return e

obj2Ent :: MonadIO m => Obj.Obj -> m E.Entity
obj2Ent o = do
  e <- E.obj2Ent o
  liftIO $ mkWeakPtr e $ Just $ E.deleteEntity e
  return e

indexedModel2Ent :: MonadIO m => [IM.IndexedModel] -> m E.Entity
indexedModel2Ent l = do
  e <- E.indexedModel2Ent l
  liftIO $ mkWeakPtr e $ Just $ E.deleteEntity e
  return e

loadShadersFromFile :: MonadIO m => [String] -> [GL.ShaderType] -> Maybe Handle -> m S.Shader
loadShadersFromFile ls lst mh = do
  s <- S.loadShadersFromFile ls lst mh
  liftIO $ mkWeakPtr s $ Just $ S.deleteShader s
  return s

loadShadersFromBS :: MonadIO m => [BS.ByteString] -> [GL.ShaderType] -> Maybe Handle -> m S.Shader
loadShadersFromBS lbs lst mh = do
  s <- S.loadShadersFromBS lbs lst mh
  liftIO $ mkWeakPtr s $ Just $ S.deleteShader s
  return s

makeMaterial :: (MonadIO m) => S.Shader -> [(FilePath,String)] -> m (Either String M.Material)
makeMaterial shader textures = do
  m <- M.makeMaterial shader textures
  forM_ m (\mat -> liftIO $ mkWeakPtr mat $ Just $ M.deleteMaterial mat)
  return m

createTexture2DStatic :: (Integral a,MonadIO m) => a -> a -> Ptr () -> T.ClampType -> T.FilteringType -> m T.Texture
createTexture2DStatic width height dataPtr ct ft = do
  t <- T.createTexture2DStatic width height dataPtr ct ft
  liftIO $ mkWeakPtr t $ Just $ T.deleteTexture t
  return t
