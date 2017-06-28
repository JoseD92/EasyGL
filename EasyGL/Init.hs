{-|
Module      : EasyGL.Init
Description : Initialize OpenGL environment.

Initialize OpenGL environment using GLUT, for aplications using a single window with a single viewport.
-}

module EasyGL.Init (MouseDelta,
  MouseKey,
  IOLoop(..),
  IORet(..),
  GLUT.MouseButton(..),
  GLUT.SpecialKey(..),
  GLUT.Key(..),
  KeyState(..),
  maybeDown,
  maybePressed,
  maybeUp,
  maybeReleased,
  initOpenGLEnvironment,
  initGL,
  currentAspect)
where

import qualified Graphics.UI.GLUT  as GLUT
import           Control.Monad
import qualified Data.ByteString           as BS
import           Data.StateVar             (get, ($=!), ($=), ($~!))
import Data.IORef (IORef, newIORef)
import           Foreign.Marshal.Array     (withArray)
import           Foreign.Storable          (Storable)
import qualified Graphics.Rendering.OpenGL as GL
import           System.IO                 (Handle, hPutStr)
import Data.Map.Strict
import qualified Data.Map.Strict as Map
import           Control.Monad.IO.Class (MonadIO,liftIO)

type MouseDelta = (GL.GLint,GL.GLint)
type MouseKey = (Map GLUT.Key KeyState,MouseDelta)
newtype IOLoop = IOLoop { runProgram :: MouseKey -> IO [IORet]}
data IORet = ChangeIOMain IOLoop |
  FixMouseAt GL.GLint GL.GLint |
  FreeMouse
  deriving (Show)

data KeyState = Down | Up | Pressed | Released deriving Show

change :: KeyState -> KeyState
change Down = Down
change Up = Up
change Released = Up
change Pressed = Down

filterDown :: t -> t -> KeyState -> t
filterDown x _ Down = x
filterDown x _ Pressed = x
filterDown _ x _ = x
filterPressed :: t -> t -> KeyState -> t
filterPressed x _ Pressed = x
filterPressed _ x _ = x
filterUp :: t -> t -> KeyState -> t
filterUp x _ Up = x
filterUp x _ Released = x
filterUp _ x _ = x
filterReleased :: t -> t -> KeyState -> t
filterReleased x _ Released = x
filterReleased _ x _ = x

maybeDown :: t -> t -> Maybe KeyState -> t
maybeDown x y = maybe x (filterDown y x)
maybePressed :: t -> t -> Maybe KeyState -> t
maybePressed x y = maybe x (filterPressed y x)
maybeUp :: t -> t -> Maybe KeyState -> t
maybeUp x y = maybe x (filterUp y x)
maybeReleased :: t -> t -> Maybe KeyState -> t
maybeReleased x y = maybe x (filterReleased y x)

-- | Returns current aspect ratio of active opengl windows.
currentAspect :: MonadIO m => m GL.GLdouble
currentAspect = do
  (GLUT.Size w h) <- GLUT.get GLUT.windowSize
  return (fromIntegral w / fromIntegral h)

instance Show IOLoop where
  show (IOLoop _) = "IOLoop"

ioRetFun :: IOLoop -> Bool -> IORef MouseKey -> IORet -> IO ()
ioRetFun _ b bottons (ChangeIOMain io) = GLUT.displayCallback $= display b bottons io
ioRetFun currentLoop _ bottons (FixMouseAt x y) = do
  GLUT.pointerPosition $=! GL.Position x y
  GLUT.passiveMotionCallback $= Just (fixedMouse x y bottons)
  GLUT.motionCallback $= Just (fixedMouse x y bottons)
  GLUT.displayCallback $= display True bottons currentLoop
  bottons $~! \(myMap,_) -> (myMap,(0,0))
ioRetFun currentLoop _ bottons FreeMouse = do
  GLUT.passiveMotionCallback $= Just (defaultMouse bottons)
  GLUT.motionCallback $= Just (defaultMouse bottons)
  GLUT.displayCallback $= display False bottons currentLoop

idle :: GLUT.IdleCallback
idle = GLUT.postRedisplay Nothing

reshape :: GLUT.ReshapeCallback
reshape size@(GL.Size w h) = do
  GL.viewport $= (GL.Position 0 0, size)
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.perspective 30 (fromIntegral w / fromIntegral h) 1 200
  GL.matrixMode $= GL.Modelview 0
  GLUT.postRedisplay Nothing

display :: Bool -> IORef MouseKey -> IOLoop -> GLUT.DisplayCallback
display b bottons io = do
  GL.clear [GL.ColorBuffer,GL.DepthBuffer]
  GL.loadIdentity
  k <- get bottons
  ret <- runProgram io k
  bottons $~! \(myMap,x) -> (fmap change myMap,if b then (0,0) else x)
  mapM_ (ioRetFun io b bottons) ret
  GLUT.swapBuffers

keyboard ::  IORef MouseKey -> GLUT.KeyboardCallback
keyboard ref c _ = do
  (myMap,delta) <- get ref
  let state = Map.lookup (GLUT.Char c) myMap
  ref $=! maybe (insert (GLUT.Char c) Pressed myMap,delta)
    (\s -> case s of
            Pressed -> (insert (GLUT.Char c) Down myMap,delta)
            Down -> (insert (GLUT.Char c) Down myMap,delta)
            _ -> (insert (GLUT.Char c) Pressed myMap,delta)
    )
    state

keyboardUp ::  IORef MouseKey -> GLUT.KeyboardCallback
keyboardUp ref c _ = ref $~! \(myMap,delta) -> (insert (GLUT.Char c) Released myMap,delta)

special ::  IORef MouseKey -> GLUT.SpecialCallback
special ref key _ = ref $~! \(myMap,delta) -> (insert (GLUT.SpecialKey  key) Pressed myMap,delta)

specialUp ::  IORef MouseKey -> GLUT.SpecialCallback
specialUp ref key _ = ref $~! \(myMap,delta) -> (insert (GLUT.SpecialKey  key) Released myMap,delta)

mouseCall ::  IORef MouseKey -> GLUT.MouseCallback
mouseCall ref key GLUT.Up _ = ref $~! \(myMap,delta) -> (insert (GLUT.MouseButton key) Released myMap,delta)
mouseCall ref key GLUT.Down _ = ref $~! \(myMap,delta) -> (insert (GLUT.MouseButton key) Pressed myMap,delta)

defaultMouse ::  IORef MouseKey -> GLUT.Position -> IO ()
defaultMouse ref (GLUT.Position x y) = ref $~! \(myMap,_) -> (myMap,(x,y))

fixedMouse :: GL.GLint -> GL.GLint -> IORef MouseKey -> GLUT.Position -> IO ()
fixedMouse fixx fixy bottons (GLUT.Position x y) = do
  bottons $~! \(myMap,_) -> (myMap,(x-fixx,y-fixy))
  GLUT.passiveMotionCallback $= Just help
  GLUT.motionCallback $= Just help
  GLUT.pointerPosition $=! GL.Position fixx fixy
  where
    help _ = do
      GLUT.passiveMotionCallback $= Just (fixedMouse fixx fixy bottons)
      GLUT.motionCallback $= Just (fixedMouse fixx fixy bottons)

initOpenGLEnvironment :: GL.GLsizei -> GL.GLsizei -> String -> IO GLUT.Window
initOpenGLEnvironment sizex sizey windowName = do
  GLUT.getArgsAndInitialize
  GLUT.initialDisplayMode $= [GLUT.DoubleBuffered,GLUT.RGBAMode,GLUT.WithDepthBuffer]
  GLUT.initialWindowSize $= GL.Size sizex sizey
  GLUT.createWindow windowName

initGL :: IOLoop -> IO ()
initGL io = do

  GL.clearColor $= GL.Color4 0 0 0 0
  GL.materialDiffuse GL.Front $= GL.Color4 1 1 1 1
  GL.materialSpecular GL.Front $= GL.Color4 1 1 1 1
  GL.materialShininess GL.Front $= 100

  GL.depthFunc $= Just GL.Less
  GL.autoNormal $= GL.Enabled
  GL.normalize $= GL.Enabled

  bottons <- newIORef ((empty,(0,0)) :: MouseKey)

  GLUT.passiveMotionCallback $= Just (defaultMouse bottons)
  GLUT.motionCallback $= Just (defaultMouse bottons)

  GLUT.keyboardCallback  $= Just (keyboard bottons)
  GLUT.keyboardUpCallback $= Just (keyboardUp bottons)
  GLUT.specialCallback $= Just (special bottons)
  GLUT.specialUpCallback $= Just (specialUp bottons)
  GLUT.mouseCallback $= Just (mouseCall bottons)

  GLUT.reshapeCallback $= Just reshape
  GLUT.idleCallback $= Just idle
  GLUT.displayCallback $= display False bottons io
  GLUT.mainLoop
