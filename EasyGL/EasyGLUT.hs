{-|
Module      : EasyGL.EasyGLUT
Description : Initialize OpenGL environment.

Initialize OpenGL environment using GLUT, for aplications using a single window with a single viewport.
-}

module EasyGL.EasyGLUT (
  MouseKey,
  MouseState(..),
  GLUT.MouseButton(..),
  GLUT.SpecialKey(..),
  GLUT.Key(..),
  KeyState(..),
  GLUT,
  changeMainLoop,
  getMouseInfo,
  getKeysInfo,
  fixMouseAt,
  freeMouse,
  currentAspect,
  getWindowSize,
  setWindowSize,
  getWindowPosition,
  setWindowPosition,
  fullScreen,
  maybeDown,
  maybePressed,
  maybeUp,
  maybeReleased,
  initOpenGLEnvironment,
  initGL)
where

import qualified Graphics.UI.GLUT  as GLUT
import           Data.StateVar             (get, ($=!), ($=), ($~!))
import Data.IORef (IORef, newIORef)
import qualified Graphics.Rendering.OpenGL as GL
import Data.Map.Strict
import qualified Data.Map.Strict as Map
import qualified Control.Monad.State.Lazy as State (get)
import Control.Monad.State.Lazy hiding (get)
import Control.Monad.Reader

-- | State of mouse.
data MouseState = FreeMouse GL.GLint GL.GLint | FixMouse GL.GLint GL.GLint

-- | Keeps info of mouse and keyboard keys state.
type MouseKey = (Map GLUT.Key KeyState,MouseState)

type GLUTState = DisplayData
type GLUTRead = MouseKey

-- | Monad that describes behavior of GLUT.
type GLUT a = StateT GLUTState (ReaderT GLUTRead IO) a

-- | Changes GLUT main callback.
changeMainLoop :: GLUT () -> GLUT ()
changeMainLoop f = modify' (\s -> s{ioLoop=f})

-- | Returns current frame mouse info.
getMouseInfo :: GLUT MouseState
getMouseInfo = fmap snd ask

-- | Returns current frame keys info.
getKeysInfo :: GLUT (Map GLUT.Key KeyState)
getKeysInfo = fmap fst ask

-- | Fix mouse at a given point of window.
fixMouseAt :: GL.GLint -> GL.GLint -> GLUT ()
fixMouseAt x y = do
  modify' (\s -> s{mouseIsFixed=True})
  bottons <- fmap bottons State.get
  GLUT.pointerPosition $=! GL.Position x y
  GLUT.passiveMotionCallback $= Just (fixedMouse x y bottons)
  GLUT.motionCallback $= Just (fixedMouse x y bottons)

-- | Allow free mouse movement.
freeMouse :: GLUT ()
freeMouse = do
  modify' (\s -> s{mouseIsFixed=False})
  bottons <- fmap bottons State.get
  GLUT.passiveMotionCallback $= Just (defaultMouse bottons)
  GLUT.motionCallback $= Just (defaultMouse bottons)

-- | Returns current aspect ratio of active glut windows.
currentAspect :: GLUT GL.GLdouble
currentAspect = do
  (GLUT.Size w h) <- GLUT.get GLUT.windowSize
  return (fromIntegral w / fromIntegral h)

-- | Returns size of active glut windows.
getWindowSize :: GLUT (GL.GLsizei,GL.GLsizei)
getWindowSize = do
  (GLUT.Size w h) <- GLUT.get GLUT.windowSize
  return (w,h)

-- | Sets size of active glut windows.
setWindowSize :: GL.GLsizei -> GL.GLsizei -> GLUT ()
setWindowSize w h = GLUT.windowSize $=! GLUT.Size w h

-- | Returns position of active glut windows.
getWindowPosition :: GLUT (GL.GLint,GL.GLint)
getWindowPosition = do
  (GLUT.Position x y) <- GLUT.get GLUT.windowPosition
  return (x,y)

-- | Sets position of active glut windows.
setWindowPosition :: GL.GLint -> GL.GLint -> GLUT ()
setWindowPosition x y = GLUT.windowPosition $=! GLUT.Position x y

-- | Makes glut window fullscreen.
fullScreen :: GLUT ()
fullScreen = GLUT.fullScreen

-- | States in witch a key can be found.
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

idle :: GLUT.IdleCallback
idle = GLUT.postRedisplay Nothing

reshape :: GLUT.ReshapeCallback
reshape size@(GL.Size w h) = do
  GL.viewport $= (GL.Position 0 0, size)
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  --GL.perspective 30 (fromIntegral w / fromIntegral h) 1 200
  --GL.matrixMode $= GL.Modelview 0
  GLUT.postRedisplay Nothing

data DisplayData = DisplayData {
    ioLoop :: GLUT (),
    mouseIsFixed :: Bool,
    bottons :: IORef MouseKey
  }
display :: DisplayData -> GLUT.DisplayCallback
display mydata = do
  GL.clear [GL.ColorBuffer,GL.DepthBuffer]
  GL.loadIdentity
  keys <- get (bottons mydata)
  s <- runReaderT (execStateT (ioLoop mydata) mydata) keys
  bottons mydata $~! \(myMap,x) -> (fmap change myMap,if mouseIsFixed mydata then FixMouse 0 0 else x)
  GLUT.swapBuffers
  GLUT.displayCallback $= display s

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
defaultMouse ref (GLUT.Position x y) = ref $~! \(myMap,_) -> (myMap,FreeMouse x y)

fixedMouse :: GL.GLint -> GL.GLint -> IORef MouseKey -> GLUT.Position -> IO ()
fixedMouse fixx fixy bottons (GLUT.Position x y) = do
  bottons $~! \(myMap,_) -> (myMap,FixMouse (x-fixx) (y-fixy))
  GLUT.passiveMotionCallback $= Just help
  GLUT.motionCallback $= Just help
  GLUT.pointerPosition $=! GL.Position fixx fixy
  where
    help _ = do
      GLUT.passiveMotionCallback $= Just (fixedMouse fixx fixy bottons)
      GLUT.motionCallback $= Just (fixedMouse fixx fixy bottons)

-- | Initializes Opengl and GLUT environments, OpenGL calls can be made after this function.
initOpenGLEnvironment :: GL.GLsizei -> GL.GLsizei -> String -> IO GLUT.Window
initOpenGLEnvironment sizex sizey windowName = do
  GLUT.getArgsAndInitialize
  GLUT.initialDisplayMode $= [GLUT.DoubleBuffered,GLUT.RGBAMode,GLUT.WithDepthBuffer]
  GLUT.initialWindowSize $= GL.Size sizex sizey
  GLUT.createWindow windowName

-- | Given a callback GLUT function, starts the main loop of program.
initGL :: GLUT () -> IO ()
initGL f = do

  GL.clearColor $= GL.Color4 0 0 0 0
  GL.materialDiffuse GL.Front $= GL.Color4 1 1 1 1
  GL.materialSpecular GL.Front $= GL.Color4 1 1 1 1
  GL.materialShininess GL.Front $= 100

  GL.depthFunc $= Just GL.Less
  GL.autoNormal $= GL.Enabled
  GL.normalize $= GL.Enabled

  bottons <- newIORef ((empty,FreeMouse 0 0) :: MouseKey)

  GLUT.passiveMotionCallback $= Just (defaultMouse bottons)
  GLUT.motionCallback $= Just (defaultMouse bottons)

  GLUT.keyboardCallback  $= Just (keyboard bottons)
  GLUT.keyboardUpCallback $= Just (keyboardUp bottons)
  GLUT.specialCallback $= Just (special bottons)
  GLUT.specialUpCallback $= Just (specialUp bottons)
  GLUT.mouseCallback $= Just (mouseCall bottons)

  GLUT.reshapeCallback $= Just reshape
  GLUT.idleCallback $= Just idle
  GLUT.displayCallback $= display (DisplayData f False bottons)
  GLUT.mainLoop
