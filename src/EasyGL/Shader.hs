--------------------------------------------------------------------------------
-- |
-- Module      :  EasyGL.Shader
-- Copyright   :  Copyright (c) 2017, Jose Daniel Duran Toro
-- License     :  BSD3
--
-- Maintainer  :  Jose Daniel Duran Toro <jose_daniel_d@hotmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Provides an easy to use interface to OpenGL shaders and programs, from creation to usage.
--
--------------------------------------------------------------------------------

module EasyGL.Shader (
  GL.ShaderType(..),
  Shader,
  loadShadersFromFile,
  loadShadersFromBS,
  deleteShader,
  withShader,
  putActiveUniforms,
  setVar,
  setArr,
  setArrLen,
  setArrPtr,
  Uniform,
  withShaderSafe,
  set,
  setF,
  setP,
  uniforms
)
where

import           Control.Monad
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import qualified Data.ByteString           as BS
import           Data.Foldable             (Foldable, toList)
import           Data.StateVar             (get, ($=!))
import           Foreign.Marshal.Array     (withArray, withArrayLen)
import           Foreign.Ptr               (Ptr)
import           Foreign.Storable          (Storable)
import qualified Graphics.Rendering.OpenGL as GL
import           System.IO                 (Handle, hPutStr)

{-|
  Defines an simple shader, that contains all compiled code gpu-ready.
-}
newtype Shader = Shader {
  program :: GL.Program}

empty :: MonadIO m => m Shader
empty = liftIO $ fmap Shader GL.createProgram

mensajes :: Maybe Handle -> GL.GettableStateVar String -> IO ()
mensajes mhandle var =
  maybe (return ())
        (\h -> do
          s <- get var
          unless (s == "\NUL" || s == "No errors.\n\NUL") $ hPutStr h s
        )
        mhandle

loadShaderFromFile :: String ->  GL.ShaderType -> IO GL.Shader
loadShaderFromFile s shadertype = do
  out <- GL.createShader shadertype
  BS.readFile s >>= (GL.shaderSourceBS out $=!)
  GL.compileShader out
  return out

loadShaderFromBS :: BS.ByteString ->  GL.ShaderType -> IO GL.Shader
loadShaderFromBS bs shadertype = do
    out <- GL.createShader shadertype
    GL.shaderSourceBS out $=! bs
    GL.compileShader out
    return out

{-|
  Creates a simple shader from files.
-}
loadShadersFromFile :: MonadIO m => [String]  -- ^ FilePaths to each shader to load.
  -> [GL.ShaderType]   -- ^ Shader type to each shader to load. See http://hackage.haskell.org/package/OpenGL-3.0.1.0/docs/Graphics-Rendering-OpenGL-GL-Shaders-ShaderObjects.html#t:ShaderType
  -> Maybe Handle   -- ^ If given a file handle, will print all compile errors to given handle
  -> m Shader
loadShadersFromFile s st mhandle = liftIO $ zipWithM loadShaderFromFile s st >>= linkShaders mhandle

{-|
  Creates a simple shader from byte strings.
-}
loadShadersFromBS :: MonadIO m => [BS.ByteString] -> [GL.ShaderType] -> Maybe Handle -> m Shader
loadShadersFromBS s st mhandle = liftIO $ zipWithM loadShaderFromBS s st >>= linkShaders mhandle

bindEasyGLShaderAttrib :: GL.Program -> IO ()
bindEasyGLShaderAttrib shaderProgram = do
  GL.attribLocation shaderProgram "position" $=! GL.AttribLocation 0
  GL.attribLocation shaderProgram "textCoord" $=! GL.AttribLocation 1
  GL.attribLocation shaderProgram "normal" $=! GL.AttribLocation 2

linkShaders :: Maybe Handle -> [GL.Shader] -> IO Shader
linkShaders mhandle shaders = do
  active <- empty
  mapM_ (GL.attachShader (program active)) shaders
  bindEasyGLShaderAttrib (program active)
  GL.linkProgram (program active)
  mapM_ (mensajes mhandle . GL.shaderInfoLog) shaders
  mapM_ GL.deleteObjectName shaders -- flags shaders for deletion
  mensajes mhandle $ GL.programInfoLog (program active)
  return active

{-|
  Frees the memory and invalidates the name associated with the shader
-}
deleteShader :: MonadIO m => Shader -> m ()
deleteShader = GL.deleteObjectName . program
-- GL.Shader objects where mark for deletion on linkShaders, no other action is required.

{-|
  Specifies an action to be made using a given shader.
-}
withShader :: MonadIO m => Shader -> m a -> m a
withShader s action = do
  pastProgram <- liftIO $ GL.get GL.currentProgram
  liftIO $ GL.currentProgram $=! Just (program s)
  ret <- action
  liftIO $ GL.currentProgram $=! pastProgram
  return ret

{-|
  Prints to stdout all active uniforms variables of currently selected shader.
-}
putActiveUniforms :: MonadIO m => m ()
putActiveUniforms = liftIO $ do
  pro <- get GL.currentProgram
  maybe (return ())
        (GL.activeUniforms >=> print)
        pro

{-|
  Sets an uniform variable of currently selected shader to a given value.
  Warning: does not checks types of provide value against variable.
-}
setVar :: (MonadIO m,GL.Uniform a) => a -> String -> m ()
setVar val str = liftIO $ do
  pro <- get GL.currentProgram
  maybe (return ())
        (\x->do
          loc <- GL.uniformLocation x str
          GL.uniform loc $=! val
        )
        pro

{-|
Sets an uniform variable of currently selected shader to a given array of values.
Warning: does not checks types of provide value against variable.
-}
setArr :: (MonadIO m,Storable  a, GL.Uniform a, Integral b) => [a] -> b -> String -> m ()
setArr vals len str = liftIO $ do
  pro <- get GL.currentProgram
  maybe (return ())
        (\x->do
          loc <- GL.uniformLocation x str
          withArray vals $ \ptr -> GL.uniformv loc (fromIntegral len) ptr
        )
        pro

{-|
Same as setArr but the number of elements is calculated.
-}
setArrLen :: (MonadIO m,Storable  a, GL.Uniform a) => [a] -> String -> m ()
setArrLen vals str = liftIO $ do
  pro <- get GL.currentProgram
  maybe (return ())
        (\x->do
          loc <- GL.uniformLocation x str
          withArrayLen vals $ \tam ptr -> GL.uniformv loc (fromIntegral tam) ptr
        )
        pro

{-|
Same as setArr but the user provides the pointer to data and then number of elements.
-}
setArrPtr :: (MonadIO m, Integral b, GL.Uniform a) => Ptr a -> b -> String -> m ()
setArrPtr ptr len str = liftIO $ do
  pro <- get GL.currentProgram
  maybe (return ())
        (\x->do
          loc <- GL.uniformLocation x str
          GL.uniformv loc (fromIntegral len) ptr
        )
        pro

--------------------------------------------------------------------------------
-- Next section describes uniform variables as a monad to increase safety.
--------------------------------------------------------------------------------

-- | Monad that describes uniform variables operations.
newtype Uniform a = Uniform { runUniform :: IO a }

instance Functor Uniform where
    fmap f (Uniform v) = Uniform (fmap f v)

instance Applicative Uniform where
    pure = Uniform . pure
    Uniform f <*> Uniform v = Uniform (f <*> v)

instance Monad Uniform where
    return = pure
    (>>=) (Uniform a) f = Uniform $ fmap f a >>= runUniform
    (>>) (Uniform a) (Uniform b) = Uniform (a >> b)

{-|
  Specifies an action to be made using a given shader.
  Safety comes from designating a monad to perform only the uniform variables operations.
-}
withShaderSafe :: MonadIO m => Shader -- ^ Shader to use.
  -> Uniform () -- ^ Uniform variables setting actions.
  -> m a -- ^ OpenGL function to render an object.
  -> m a
withShaderSafe s u action = do
  pastProgram <- liftIO $ GL.get GL.currentProgram
  liftIO $ GL.currentProgram $=! Just (program s)
  liftIO $ runUniform u
  ret <- action
  liftIO $ GL.currentProgram $=! pastProgram
  return ret

-- | Sets an uniform variable to a value.
set :: GL.Uniform a => String -> a -> Uniform ()
set str a = Uniform $ setVar a str

-- | Sets an uniform array to the values in a foldable container.
setF :: (Foldable t,Storable  a, GL.Uniform a) => String -> t a -> Uniform ()
setF str container = Uniform $ setArrLen (toList container) str

-- | Given a pointer to data and the number of elements, sets an uniform array to data in pointer.
setP :: (Integral b, GL.Uniform a) => String -> Ptr a -> b -> Uniform ()
setP str ptr len = Uniform $ setArrPtr ptr len str

-- | Prints the uniform variables of a shader.
uniforms :: Uniform ()
uniforms = Uniform putActiveUniforms
