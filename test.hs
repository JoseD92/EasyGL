import EasyGL.Init
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
import EasyGL.Shader (setVar)
import EasyGL.Obj.Obj (readObj,toIndexedModel,toIndexedModel)
import qualified EasyGL.Obj.ObjData as O
import qualified EasyGL.IndexedModel as IM
import Data.List (nub)
import DeltaClock

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

main = do
  initOpenGLEnvironment 800 600 "test"
  let (Right c) = createCamera3D 0.0 0.0 10.0 0 0 0 30 (800/600) 0.3 200
  mydata <- newIORef $ MyData c False 0 0 0 0
  myShader <- S.loadShadersFromFile ["./testAssets/3Dshaders/vertex.shader","./testAssets/3Dshaders/frag.shader"] [VertexShader,FragmentShader] $ Just stderr
  --a <- makeMaterial "mat1" myShader [("./Young Link/YoungLink_grp1.png","sampler01")]
  a <- makeMaterial "mat1" myShader []
  clock <- initClock >>= newIORef
  case a of
    Left s -> putStrLn s
    Right mat -> do
      putStrLn "Loading"
      obj <- readObj <$> readFile "./testAssets/sphere.obj"--"./Young Link/YoungLink.obj"
      let im = IM.generateNormalsHard $ toIndexedModel $ head $ O.groups obj --IM.generateNormalsSoft
      ent <- indexedModel2Ent [im]
      putStrLn "Loaded"

      initGL $ IOLoop $ myfun (getDelta clock) (mat,ent) mydata

sensitivity = 0.1

sumWithLimits max min a b
  | a + b > max = max
  | a + b < min = min
  | otherwise = a + b

sumCiclic max a b
  | a + b > max = a + b - max
  | a + b < 0 = a + b + max
  | otherwise = a + b

myfun :: IO Double -> (Material,Entity) -> IORef MyData -> MouseKey -> IO [IORet]
myfun clock (mat,link) mydataref a@(myMap,(deltax,deltay)) = do
  mydata <- get mydataref

  putStrLn $ (show . yaw) mydata ++ " " ++ (show . picth) mydata

  useCamera $ camera mydata
  maybeDown (return ()) (do
      lookAt (Vertex3 0.0 0.0 10.0) (Vertex3 (-1.836909530733566e-16) 6.123031769111886e-17 9.0) (Vector3 0.0 1.0 0.0)
      print "r"
    ) $ Map.lookup (Char 'r') myMap

  color $ Color3 (1::GLfloat) 1 1 -- set outline color to black
  cubeFrame 1 -- draw the outline

  preservingMatrix $ do
    scale 20 20 (20 :: GLfloat)
    --color $ Color3 (0::GLfloat) 1 0
    --normals
    drawWithMat mat link $ do
      --print $ shaderUse mydata
      setVar (shaderUse mydata) "use"

  deltaTime <- fmap (*5) clock
  --controls
  when (vistaEneabled mydata) $ do
    mydataref $~! \ref ->
      ref{yaw= sumWithLimits 90 (-90) (-1 * fromIntegral deltay * sensitivity) (yaw ref),
          picth= sumCiclic 360 (-1 * fromIntegral deltax * sensitivity) (picth ref),
          roll=roll ref
        }
    mydataref $~! updateRot

  case exit of
    Just Down -> exitSuccess
    _ -> return ()

  mydataref $~! \ref -> ref{camera=translateCamera (mvx*deltaTime) (mvy*deltaTime) (mvz*deltaTime) $ camera ref}

  maybePressed (return ()) (mydataref $~! \ref -> ref{shaderUse=mod (shaderUse ref+1) 3}) $ Map.lookup (Char 'e') myMap
  maybePressed (return ()) (polygonMode $=! (Point,Point)) $ Map.lookup (Char 'u') myMap
  maybePressed (return ()) (polygonMode $=! (Line,Line)) $ Map.lookup (Char 'i') myMap
  maybePressed (return ()) (polygonMode $=! (Fill,Fill)) $ Map.lookup (Char 'o') myMap
  maybePressed (return ()) (get polygonMode >>= print) $ Map.lookup (Char 'p') myMap

  case lockCamera of
    Just Down -> do
      mydataref $~! \ref -> ref{vistaEneabled=True}
      return [FixMouseAt 400 300]
    _ -> case freeCamera of
      Just Down -> do
        mydataref $~! \ref -> ref{vistaEneabled=False}
        return [FreeMouse]
      _ -> return []
  where
    exit = Map.lookup (Char '\ESC') myMap
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
