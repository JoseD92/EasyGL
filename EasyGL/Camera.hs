{-|
Module      : EasyGL.Camera
Description : Helps by providing an easy to use interface to OpenGL perspectives.

Provides an easy to use interface to OpenGL perspectives and change them easily.
-}
module EasyGL.Camera (
  Camera,
  useCamera,
  createCamera2D,
  createCamera3D,
  yawCamera,
  picthCamera,
  rollCamera,
  setYawPitchRoll,
  currentAspect
)
where
import           Control.Monad.IO.Class (MonadIO,liftIO)
import qualified Graphics.GLU as GLU
import qualified Graphics.GL as GL
import Graphics.GL (GLdouble)
import qualified Data.Matrix as Mat
import Foreign.Marshal.Array
import qualified Graphics.UI.GLUT as GLUT

{-|
  Stores OpenGL data required to change perspectives.
-}
data Camera = Camera2D GLdouble GLdouble GLdouble GLdouble GLdouble GLdouble
  | Camera3D !(Mat.Matrix GLdouble) GLdouble GLdouble GLdouble GLdouble GLdouble GLdouble GLdouble

{-|
  Sets OpenGL environment to use given camera.
  this operation changes the current transformation matrix.
-}
useCamera :: MonadIO m => Camera -> m ()
useCamera (Camera2D left right bottom top near far) = GL.glOrtho left right bottom top near far
useCamera (Camera3D matrix x y z fovy aspect zNear zFar) = do
  GL.glLoadIdentity
  GLU.gluPerspective fovy aspect zNear zFar
  liftIO $ withArray (Mat.toList matrix) $ \ptr -> GL.glMultMatrixd ptr
  GL.glTranslated (-x) (-y) (-z)

{-|
  Creates 2D camera.
-}
createCamera2D :: GLdouble -- ^ Specify the coordinates for the left clipping plane.
  -> GLdouble -- ^ Specify the coordinates for the right clipping plane.
  -> GLdouble -- ^ Specify the coordinates for the bottom clipping plane.
  -> GLdouble -- ^ Specify the coordinates for the top clipping plane.
  -> GLdouble -- ^ Specify the distances to the nearer depth clipping plane.
  -> GLdouble -- ^ Specify the distances to the farther depth clipping plane.
  -> Camera
createCamera2D = Camera2D

{-|
  Creates 3D camera.
-}
createCamera3D :: GLdouble -- ^ Position of camera in x.
  -> GLdouble -- ^ Position of camera in y.
  -> GLdouble -- ^ Position of camera in z.
  -> GLdouble -- ^ Camera Picth.
  -> GLdouble -- ^ Camera Roll.
  -> GLdouble -- ^ Camera Yaw.
  -> GLdouble -- ^ Specifies the field of view angle, in degrees, in the y direction.
  -> GLdouble -- ^ Specifies the aspect ratio that determines the field of view in the x direction. The aspect ratio is the ratio of x (width) to y (height).
  -> GLdouble -- ^ Specifies the distance from the Camera to the near clipping plane (always positive).
  -> GLdouble -- ^ Specifies the distance from the Camera to the far clipping plane (always positive).
  -> Either String Camera
createCamera3D x y z picth roll yaw fovy aspect zNear zFar
  | zNear > 0 && zFar > 0 = Right $ Camera3D rot x y z fovy aspect zNear zFar
  | otherwise = Left "zNear or zFar is not positive."
  where
    rot = yawPicthRollMatrix yaw picth roll

-- | Returns current aspect ratio of active opengl windows.
currentAspect :: MonadIO m => m GLdouble
currentAspect = do
  (GLUT.Size w h) <- GLUT.get GLUT.windowSize
  return (fromIntegral w / fromIntegral h)

-- | Sets camera yaw picth and roll
setYawPitchRoll :: GLdouble -> GLdouble -> GLdouble -> Camera -> Camera
setYawPitchRoll _ _ _ c@Camera2D{} = c
setYawPitchRoll yaw picth roll (Camera3D _ x y z fovy aspect zNear zFar) = Camera3D newMat x y z fovy aspect zNear zFar
  where
    newMat = yawPicthRollMatrix yaw picth roll

-- | Yaws Camera
yawCamera :: GLdouble -> Camera -> Camera
yawCamera _ c@Camera2D{} = c
yawCamera yaw (Camera3D mat x y z fovy aspect zNear zFar) = Camera3D newMat x y z fovy aspect zNear zFar
  where
    newMat = Mat.multStd (yawMatrix yaw) mat

-- | Picthes Camera
picthCamera :: GLdouble -> Camera -> Camera
picthCamera _ c@Camera2D{} = c
picthCamera picth (Camera3D mat x y z fovy aspect zNear zFar) = Camera3D newMat x y z fovy aspect zNear zFar
  where
    newMat = Mat.multStd (picthMatrix picth) mat

-- | Rolls Camera
rollCamera :: GLdouble -> Camera -> Camera
rollCamera _ c@Camera2D{} = c
rollCamera roll (Camera3D mat x y z fovy aspect zNear zFar) = Camera3D newMat x y z fovy aspect zNear zFar
  where
    newMat = Mat.multStd (rollMatrix roll) mat

toAngle :: Floating a => a -> a
toAngle a = pi*a/180

sina :: Floating a => a -> a
sina = sin . toAngle

cosa :: Floating a => a -> a
cosa = cos . toAngle

yawMatrix :: Floating a => a -> Mat.Matrix a
yawMatrix yaw = Mat.fromList 4 4 [
    1, 0, 0, 0,
    0, cosa yaw, -1 * sina yaw, 0,
    0, sina yaw, cosa yaw, 0,
    0, 0, 0, 0
  ]

picthMatrix :: Floating a => a -> Mat.Matrix a
picthMatrix picth = Mat.fromList 4 4 [
    cosa picth, 0, sina picth, 0,
    0, 1, 0, 0,
    -1 * sina picth, 0, cosa picth, 0,
    0, 0, 0, 0
  ]

rollMatrix :: Floating a => a -> Mat.Matrix a
rollMatrix roll = Mat.fromList 4 4 [
    cosa roll, -1 * sina roll, 0, 0,
    sina roll, cosa roll, 0, 0,
    0, 0, 0, 0,
    0, 0, 0, 0
  ]

yawPicthRollMatrix :: Floating a => a -> a -> a -> Mat.Matrix a
yawPicthRollMatrix yaw picth roll = Mat.multStd (rollMatrix roll) $ Mat.multStd (picthMatrix picth) (yawMatrix yaw)
