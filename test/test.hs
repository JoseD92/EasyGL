import EasyGL.EasyGLUT
import qualified EasyGL.Shader as S
import qualified Data.Map as Map
import Control.Monad
import System.Exit
import Graphics.Rendering.OpenGL
import           Data.StateVar             (get, ($=!), ($=), ($~!))
import Data.IORef (IORef, newIORef)
import System.IO (stderr)
import EasyGL.Entity
import EasyGL.Camera
import EasyGL.Material
import EasyGL.Obj
import qualified EasyGL.IndexedModel as IM
import Data.List (nub)
import DeltaClock
import           Control.Monad.IO.Class (MonadIO,liftIO)

vertex3f (x,y,z) = vertex $ Vertex3 x y z
vecSum (Vertex3 x1 y1 z1) (Vertex3 x2 y2 z2) = Vertex3 (x1+x2) (y1+y2) (z1+z2)

cubeFrame :: GLfloat -> IO ()
cubeFrame w = renderPrimitive Lines $ mapM_ vertex3f
  [ ( w,-w, w), ( w, w, w),  ( w, w, w), (-w, w, w),
    (-w, w, w), (-w,-w, w),  (-w,-w, w), ( w,-w, w),
    ( w,-w, w), ( w,-w,-w),  ( w, w, w), ( w, w,-w),
    (-w, w, w), (-w, w,-w),  (-w,-w, w), (-w,-w,-w),
    ( w,-w,-w), ( w, w,-w),  ( w, w,-w), (-w, w,-w),
    (-w, w,-w), (-w,-w,-w),  (-w,-w,-w), ( w,-w,-w) ]

cambio angulo movimiento max sensibilidad
  | d >= max = 0
  | d <= 0 = max - d
  | otherwise = d
  where
    d = angulo + fromIntegral movimiento/sensibilidad

data MyData = MyData {
  camera :: Camera3D,
  vistaEneabled :: Bool,
  shaderUse :: GLint,
  yaw :: GLdouble,
  picth :: GLdouble,
  roll :: GLdouble
}

updateRot m = m{camera=setYawPitchRoll (yaw m) (picth m) (roll m) $ camera m}

sphere :: S.Shader -> IO (Material,Entity,IO ())
sphere myShader = do
  m <- makeMaterial myShader []
  case m of
    Left s -> putStrLn s >> exitSuccess
    Right mat -> do
      obj <- readObj <$> readFile "./testAssets/sphere.obj"
      e <- indexedModel2Ent $ map IM.generateNormalsHard $ toIndexedModel obj
      return (mat,e,scale 20 20 (20 :: GLfloat))

armadillo :: S.Shader -> IO (Material,Entity,IO ())
armadillo myShader = do
  m <- makeMaterial myShader []
  case m of
    Left s -> putStrLn s >> exitSuccess
    Right mat -> do
      e <- readObj2Ent "./testAssets/armadillo.obj"
      return (mat,e,return ())

link :: S.Shader -> IO (Material,Entity,IO ())
link myShader = do
  m <- makeMaterial myShader [("./testAssets/Young Link/YoungLink_grp1.png","sampler01")]
  case m of
    Left s -> putStrLn s >> exitSuccess
    Right mat -> do
      e <- readObj2Ent "./testAssets/Young Link/YoungLink.obj"
      return (mat,e,scale 0.1 0.1 (0.1 :: GLfloat))

eagle :: S.Shader -> IO (Material,Entity,IO ())
eagle myShader = do
  m <- makeMaterial myShader [("./testAssets/Eagle/texture.jpg","sampler01")]
  case m of
    Left s -> putStrLn s >> exitSuccess
    Right mat -> do
      e <- readObj2Ent "./testAssets/Eagle/eagle.obj"
      return (mat,e,scale 0.1 0.1 (0.1 :: GLfloat) >> translate (Vector3 (-35) (-66) (-442 :: GLfloat)) )

main = do
  initOpenGLEnvironment 800 600 "test"
  let (Right c) = createCamera3D 0.0 0.0 10.0 0 0 0 30 (800/600) 0.3 200
  mydata <- newIORef $ MyData c False 0 0 0 0
  clock <- initClock >>= newIORef
  putStrLn "Loading"
  myShader <- S.loadShadersFromFile ["./testAssets/3Dshaders/vertex.shader","./testAssets/3Dshaders/frag.shader"] [VertexShader,FragmentShader] $ Just stderr
  stuff <- armadillo myShader
  putStrLn "Loaded"

  initGL $ myfun (getDelta clock) stuff mydata

sensitivity = 0.1

sumWithLimits max min a b
  | a + b > max = max
  | a + b < min = min
  | otherwise = a + b

sumCiclic max a b
  | a + b > max = a + b - max
  | a + b < 0 = a + b + max
  | otherwise = a + b

myfun :: IO Double -> (Material,Entity,IO ()) -> IORef MyData -> GLUT ()
myfun clock (mat,link,scaling) mydataref = do
  myMap <- getKeysInfo
  mouse <- getMouseInfo
  mydata <- get mydataref

  let exit = Map.lookup (Char '\ESC') myMap
      lockCamera = Map.lookup (Char 'z') myMap
      freeCamera = Map.lookup (Char 'x') myMap
      pressUp = maybeDown (0,0,0) (0,0,1) $ Map.lookup (Char 'w') myMap
      pressDown = maybeDown (0,0,0) (0,0,-1) $ Map.lookup (Char 's') myMap
      pressLeft = maybeDown (0,0,0) (-1,0,0) $ Map.lookup (Char 'a') myMap
      pressRight = maybeDown (0,0,0) (1,0,0) $ Map.lookup (Char 'd') myMap
      pressSpace = maybeDown (0,0,0) (0,1,0) $ Map.lookup (Char ' ') myMap
      pressLControl = maybeDown (0,0,0) (0,-1,0) $ Map.lookup (Char 'q') myMap
      tripletSum (x1,y1,z1) (x2,y2,z2) = (x1+x2,y1+y2,z1+z2)
      (mvx,mvy,mvz) = foldr tripletSum (0,0,0) [pressUp,pressDown,pressLeft,pressRight,pressSpace,pressLControl]

  liftIO $ do
    putStrLn $ (show . yaw) mydata ++ " " ++ (show . picth) mydata

    useCamera $ camera mydata
    maybeDown (return ()) (do
        lookAt (Vertex3 0.0 0.0 10.0) (Vertex3 (-1.836909530733566e-16) 6.123031769111886e-17 9.0)  (Vector3 0.0 1.0 0.0)
        print "r"
      ) $ Map.lookup (Char 'r') myMap

    color $ Color3 (1::GLfloat) 1 1 -- set outline color to black
    cubeFrame 1 -- draw the outline

    preservingMatrix $ do
      scaling
      drawWithMat mat link $
        S.set "use" (shaderUse mydata)

  deltaTime <- fmap (*5) $ liftIO clock

  --controls
  when (vistaEneabled mydata) $ do
      (FixMouse deltax deltay) <- getMouseInfo
      liftIO $ do
        mydataref $~! \ref ->
          ref{picth= sumWithLimits 90 (-90) (-1 * fromIntegral deltay * sensitivity) (picth ref),
              yaw= sumCiclic 360 (-1 * fromIntegral deltax * sensitivity) (yaw ref)
            }
        maybeDown (return ()) (mydataref $~! \ref -> ref{roll=roll ref + 10 * deltaTime}) $ Map.lookup (Char 'g') myMap
        maybeDown (return ()) (mydataref $~! \ref -> ref{roll=roll ref - 10 * deltaTime}) $ Map.lookup (Char 'f') myMap
        mydataref $~! updateRot

  liftIO $ case exit of
    Just Down -> exitSuccess
    _ -> return ()

  liftIO $ do
    mydataref $~! \ref -> ref{camera=translateCamera (mvx*deltaTime) (mvy*deltaTime) (mvz*deltaTime) $ camera ref}

    maybePressed (return ()) (mydataref $~! \ref -> ref{shaderUse=mod (shaderUse ref+1) 3}) $ Map.lookup (Char 'e') myMap
    maybePressed (return ()) (polygonMode $=! (Point,Point)) $ Map.lookup (Char 'u') myMap
    maybePressed (return ()) (polygonMode $=! (Line,Line)) $ Map.lookup (Char 'i') myMap
    maybePressed (return ()) (polygonMode $=! (Fill,Fill)) $ Map.lookup (Char 'o') myMap
    maybePressed (return ()) (get polygonMode >>= print) $ Map.lookup (Char 'p') myMap

  case lockCamera of
    Just Down -> do
      mydataref $~! \ref -> ref{vistaEneabled=True}
      fixMouseAt 400 300
      hideCursor
    _ -> case freeCamera of
      Just Down -> do
        mydataref $~! \ref -> ref{vistaEneabled=False}
        freeMouse
        showCursor
      _ -> return ()
