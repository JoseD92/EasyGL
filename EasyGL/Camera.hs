{-|
Module      : EasyGL.Camera
Description : Helps by providing an easy to use interface to OpenGL perspectives.

Provides an easy to use interface to OpenGL perspectives and change them easily.
-}
module EasyGL.Camera (
  Camera2D,
  Camera3D,
  useCamera,
  createCamera2D,
  createCamera3D,
  yawCamera,
  picthCamera,
  rollCamera,
  setYawPitchRoll,
  setPosition,
  translateCamera
)
where
import           Control.Monad.IO.Class (MonadIO,liftIO)
import qualified Graphics.GLU as GLU
import qualified Graphics.GL as GL
import Graphics.GL (GLdouble)
import qualified Data.Matrix as Mat
import Foreign.Marshal.Array

{-|
  Sets OpenGL environment to use given camera.
-}
class IsCamera c where
  useCamera :: MonadIO m => c -> m ()

{-|
  Stores OpenGL data required to generate a 2d perspective.
-}
data Camera2D = Camera2D GLdouble GLdouble GLdouble GLdouble GLdouble GLdouble

instance IsCamera Camera2D where
  useCamera (Camera2D left right bottom top near far) = GL.glOrtho left right bottom top near far

{-|
  Creates 2D camera.
-}
createCamera2D :: GLdouble -- ^ Specify the coordinates for the left clipping plane.
  -> GLdouble -- ^ Specify the coordinates for the right clipping plane.
  -> GLdouble -- ^ Specify the coordinates for the bottom clipping plane.
  -> GLdouble -- ^ Specify the coordinates for the top clipping plane.
  -> GLdouble -- ^ Specify the distances to the nearer depth clipping plane.
  -> GLdouble -- ^ Specify the distances to the farther depth clipping plane.
  -> Camera2D
createCamera2D = Camera2D

{-|
  Stores OpenGL data required to generate a 3d perspective.
-}
data Camera3D = Camera3D !(Mat.Matrix GLdouble) GLdouble GLdouble GLdouble GLdouble GLdouble GLdouble GLdouble

instance IsCamera Camera3D where
  useCamera (Camera3D matrix x y z fovy aspect zNear zFar) = do
    GL.glLoadIdentity
    GLU.gluPerspective fovy aspect zNear zFar
    GL.glMatrixMode GL.GL_MODELVIEW
    GL.glLoadIdentity
    liftIO $ withArray (Mat.toList matrix) $ \ptr -> GL.glMultMatrixd ptr
    GL.glTranslated (-x) (-y) (-z)

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
  -> Either String Camera3D
createCamera3D x y z yaw picth roll fovy aspect zNear zFar
  | zNear > 0 && zFar > 0 = Right $ Camera3D rot x y z fovy aspect zNear zFar
  | otherwise = Left "zNear or zFar is not positive."
  where
    rot = yawPicthRollMatrix yaw picth roll

-- | Sets camera yaw picth and roll
setYawPitchRoll :: GLdouble -> GLdouble -> GLdouble -> Camera3D -> Camera3D
setYawPitchRoll yaw picth roll (Camera3D _ x y z fovy aspect zNear zFar) = Camera3D newMat x y z fovy aspect zNear zFar
  where
    newMat = yawPicthRollMatrix yaw picth roll

-- | Sets camera position (x,y,z)
setPosition :: GLdouble -> GLdouble -> GLdouble -> Camera3D -> Camera3D
setPosition x y z (Camera3D mat _ _ _ fovy aspect zNear zFar) = Camera3D mat x y z fovy aspect zNear zFar

-- | translates camera position (x,y,z)
translateCamera :: GLdouble -> GLdouble -> GLdouble -> Camera3D -> Camera3D
translateCamera x y z (Camera3D mat oldx oldy oldz fovy aspect zNear zFar) = Camera3D mat (x+oldx) (y+oldy) (z+oldz) fovy aspect zNear zFar

-- | Yaws Camera
yawCamera :: GLdouble -> Camera3D -> Camera3D
yawCamera yaw (Camera3D mat x y z fovy aspect zNear zFar) = Camera3D newMat x y z fovy aspect zNear zFar
  where
    newMat = Mat.multStd (yawMatrix yaw) mat

-- | Picthes Camera
picthCamera :: GLdouble -> Camera3D -> Camera3D
picthCamera picth (Camera3D mat x y z fovy aspect zNear zFar) = Camera3D newMat x y z fovy aspect zNear zFar
  where
    newMat = Mat.multStd (picthMatrix picth) mat

-- | Rolls Camera
rollCamera :: GLdouble -> Camera3D -> Camera3D
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
    0, 0, 0, 1
  ]

picthMatrix :: Floating a => a -> Mat.Matrix a
picthMatrix picth = Mat.fromList 4 4 [
    cosa picth, 0, sina picth, 0,
    0, 1, 0, 0,
    -1 * sina picth, 0, cosa picth, 0,
    0, 0, 0, 1
  ]

rollMatrix :: Floating a => a -> Mat.Matrix a
rollMatrix roll = Mat.fromList 4 4 [
    cosa roll, -1 * sina roll, 0, 0,
    sina roll, cosa roll, 0, 0,
    0, 0, 1, 0,
    0, 0, 0, 1
  ]

yawPicthRollMatrix :: Floating a => a -> a -> a -> Mat.Matrix a
yawPicthRollMatrix yaw picth roll = Mat.multStd (rollMatrix roll) $ Mat.multStd (picthMatrix picth) (yawMatrix yaw)
