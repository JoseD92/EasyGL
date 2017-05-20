module EasyGL.IndexedModel (
  IndexedModel(..),
  emptyIndexedModel,
  renderNormals,
  generateNormalsSoft,
  generateNormalsHard
)
where

import Graphics.Rendering.OpenGL hiding (get)
import EasyGL.Util
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import Data.Foldable (toList)
import Data.Vector.Storable ((!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Control.Monad.ST
import Control.Monad.State.Lazy
import Control.Monad.Reader

data IndexedModel = IndexedModel {
  vertices :: !(VS.Vector (Vertex3 GLfloat)),
  normals :: !(VS.Vector (Vector3 GLfloat)),
  textureCoord :: !(VS.Vector (Vector2 GLfloat)),
  indexes :: !(VS.Vector GLuint)
} deriving (Show)

emptyIndexedModel :: IndexedModel
emptyIndexedModel = IndexedModel VS.empty VS.empty VS.empty VS.empty

toVector :: (Num a) => Vertex3 a -> Vertex3 a -> Vector3 a
toVector (Vertex3 x1 y1 z1)  (Vertex3 x2 y2 z2) = Vector3 (x1-x2) (y1-y2) (z1-z2)

toVertex :: (Num a) => Vertex3 a -> Vector3 a -> Vertex3 a
toVertex (Vertex3 x1 y1 z1)  (Vector3 x2 y2 z2) = Vertex3 (x1+x2) (y1+y2) (z1+z2)

renderLine :: Vertex3 GLfloat -> Vertex3 GLfloat -> IO ()
renderLine a b = do
  vertex a
  vertex b

renderNormals :: IndexedModel -> IO ()
renderNormals g = renderPrimitive Lines $ VS.zipWithM_ renderLine verts newVerts
  where
    verts = vertices g
    newVerts = VS.zipWith toVertex verts (normals g)


--Normals generation
generateNormalsSoft :: IndexedModel -> IndexedModel
generateNormalsSoft g = g{normals=runST $ do
    acc <- VM.replicate len Nothing
    runReaderT (evalStateT (generateNormalsSoftAux g) 0) acc
    fmap (VS.concat . map (VS.singleton . maybeNormalize) . toList) $ V.freeze acc
  }
  where
    len = VS.length . vertices $ g
    maybeNormalize Nothing = Vector3 0 0 0
    maybeNormalize (Just v) = normalizeVec3 v

(<+>) :: (Num a) => a -> Maybe a -> Maybe a
e <+> Nothing = Just e
e <+> (Just a) = Just $ a+e

generateNormalsSoftAux :: IndexedModel -> StateT Int (ReaderT (VM.MVector s (Maybe (Vector3 GLfloat))) (ST s)) ()
generateNormalsSoftAux g = do
  acc <- ask
  current <- get
  unless (current == len) $ do
    let x = fromIntegral $ index ! current
        y = fromIntegral $ index ! (current+1)
        z = fromIntegral $ index ! (current+2)
        v1 = toVector (verts ! y) (verts ! x)
        v2 = toVector (verts ! z) (verts ! x)
        normal = normalizeVec3 $ crossVec3 v1 v2
    VM.modify acc (normal <+>) x
    VM.modify acc (normal <+>) y
    VM.modify acc (normal <+>) z
    put $ current+3
    generateNormalsSoftAux g
  where
    len = VS.length . indexes $ g
    verts = vertices g
    index = indexes g

generateNormalsHard :: IndexedModel -> IndexedModel
generateNormalsHard g = runST $ do
  inde <- VS.thaw $ indexes g
  acc <- VM.replicate len Nothing
  if VS.null texts then do
    (extraVerts,extraNorms) <- runReaderT (evalStateT generateNormalsHardAux1 (Hard1 0 len Map.empty Seq.empty Seq.empty)) (verts,inde,acc)
    let newVerts = verts VS.++ (VS.concat . map VS.singleton . toList $ extraVerts)
    newInde <- VS.freeze inde
    newNorms0 <- fmap (VS.concat . map (VS.singleton . toVer) . toList) $ V.freeze acc
    let newNorms = newNorms0 VS.++ (VS.concat . map VS.singleton . toList $ extraNorms)
    return $ IndexedModel newVerts newNorms VS.empty newInde
  else do
    (extraVerts,extraNorms,extraText) <- runReaderT (evalStateT generateNormalsHardAux2 (Hard2 0 len Map.empty Seq.empty Seq.empty Seq.empty)) (verts,texts,inde,acc)
    let newVerts = verts VS.++ (VS.concat . map VS.singleton . toList $ extraVerts)
    newInde <- VS.freeze inde
    let newText = texts VS.++ (VS.concat . map VS.singleton . toList $ extraText)
    newNorms0 <- fmap (VS.concat . map (VS.singleton . toVer) . toList) $ V.freeze acc
    let newNorms = newNorms0 VS.++ (VS.concat . map VS.singleton . toList $ extraNorms)
    return $ IndexedModel newVerts newNorms newText newInde
  where
    toVer Nothing = Vector3 0 0 0
    toVer (Just v) = v
    verts = vertices g
    texts = textureCoord g
    len = VS.length verts

data Hard1 = Hard1 {
    current1 :: Int,
    next1 :: Int,
    extraMap1 :: Map.Map (Vertex3 GLfloat,Vector3 GLfloat) GLuint,
    extraVert1 :: Seq.Seq (Vertex3 GLfloat),
    extraNorm1 :: Seq.Seq (Vector3 GLfloat)
  }

generateNormalsHardAux1 :: StateT Hard1 (ReaderT (VS.Vector (Vertex3 GLfloat),VSM.MVector s GLuint,VM.MVector s (Maybe (Vector3 GLfloat))) (ST s)) (Seq.Seq (Vertex3 GLfloat),Seq.Seq (Vector3 GLfloat))
generateNormalsHardAux1 = do
  (verts,index,normacc) <- ask
  data1 <- get
  let current = current1 data1
      emap = extraMap1 data1
  if current == VSM.length index then return (extraVert1 data1,extraNorm1 data1)
  else do
    x <- fmap fromIntegral $ VSM.read index current
    y <- fmap fromIntegral $ VSM.read index (current+1)
    z <- fmap fromIntegral $ VSM.read index (current+2)
    currentNorm1 <- VM.read normacc x
    currentNorm2 <- VM.read normacc y
    currentNorm3 <- VM.read normacc z
    let vert1 = verts ! x
        vert2 = verts ! y
        vert3 = verts ! z
        v1 = toVector vert2 vert1
        v2 = toVector vert3 vert1
        normal = normalizeVec3 $ crossVec3 v1 v2
        mextra1 = Map.lookup (vert1,normal) emap
        mextra2 = Map.lookup (vert2,normal) emap
        mextra3 = Map.lookup (vert3,normal) emap
    decider1 0 x normal currentNorm1 mextra1
    decider1 1 y normal currentNorm2 mextra2
    decider1 2 z normal currentNorm3 mextra3
    modify $ \data2 -> data2{current1=current+3}
    generateNormalsHardAux1

decider1 :: Int -> Int -> Vector3 GLfloat -> Maybe (Vector3 GLfloat) -> Maybe GLuint -> StateT Hard1 (ReaderT (VS.Vector (Vertex3 GLfloat),VSM.MVector s GLuint,VM.MVector s (Maybe (Vector3 GLfloat))) (ST s)) ()
decider1 offset pos norm currentNorm mextra = do
  (verts,index,normacc) <- ask
  data1 <- get
  let current = current1 data1
      next = next1 data1
      emap = extraMap1 data1
      extraV = extraVert1 data1
      extraN = extraNorm1 data1
      vert = verts ! pos
  maybe (do
        VM.write normacc pos (Just norm)
        modify $ \data2 -> data2{extraMap1=Map.insert (vert,norm) (fromIntegral pos) emap}
    )
    (const $ maybe (do
        VSM.write index (current+offset) (fromIntegral next)
        modify $ \data2 -> data2{next1=next + 1,
          extraVert1=extraV Seq.|> vert,
          extraNorm1=extraN Seq.|> norm,
          extraMap1=Map.insert (vert,norm) (fromIntegral next) emap}
      )
      (\i->VSM.write index (current+offset) (fromIntegral i))
      mextra
    )
    currentNorm

data Hard2 = Hard2 {
    current2 :: Int,
    next2 :: Int,
    extraMap2 :: Map.Map (Vertex3 GLfloat,Vector2 GLfloat,Vector3 GLfloat) GLuint,
    extraVert2 :: Seq.Seq (Vertex3 GLfloat),
    extraNorm2 :: Seq.Seq (Vector3 GLfloat),
    extraText2 :: Seq.Seq (Vector2 GLfloat)
  }

generateNormalsHardAux2 :: StateT Hard2 (ReaderT (VS.Vector (Vertex3 GLfloat),VS.Vector (Vector2 GLfloat),VSM.MVector s GLuint,VM.MVector s (Maybe (Vector3 GLfloat))) (ST s)) (Seq.Seq (Vertex3 GLfloat),Seq.Seq (Vector3 GLfloat),Seq.Seq (Vector2 GLfloat))
generateNormalsHardAux2 = do
  (verts,texts,index,normacc) <- ask
  data1 <- get
  let current = current2 data1
      emap = extraMap2 data1
  if (current == VSM.length index) then return (extraVert2 data1,extraNorm2 data1,extraText2 data1)
  else do
    x <- fmap fromIntegral $ VSM.read index current
    y <- fmap fromIntegral $ VSM.read index (current+1)
    z <- fmap fromIntegral $ VSM.read index (current+2)
    currentNorm1 <- VM.read normacc x
    currentNorm2 <- VM.read normacc y
    currentNorm3 <- VM.read normacc z
    let vert1 = verts ! x
        vert2 = verts ! y
        vert3 = verts ! z
        text1 = texts ! x
        text2 = texts ! y
        text3 = texts ! z
        v1 = toVector vert2 vert1
        v2 = toVector vert3 vert1
        normal = normalizeVec3 $ crossVec3 v1 v2
        mextra1 = Map.lookup (vert1,text1,normal) emap
        mextra2 = Map.lookup (vert2,text2,normal) emap
        mextra3 = Map.lookup (vert3,text3,normal) emap
    decider2 0 x normal currentNorm1 mextra1
    decider2 1 y normal currentNorm2 mextra2
    decider2 2 z normal currentNorm3 mextra3
    modify $ \data2 -> data2{current2=current+3}
    generateNormalsHardAux2

decider2 :: Int -> Int -> Vector3 GLfloat -> Maybe (Vector3 GLfloat) -> Maybe GLuint -> StateT Hard2 (ReaderT (VS.Vector (Vertex3 GLfloat),VS.Vector (Vector2 GLfloat),VSM.MVector s GLuint,VM.MVector s (Maybe (Vector3 GLfloat))) (ST s)) ()
decider2 offset pos norm currentNorm mextra = do
  (verts,texts,index,normacc) <- ask
  data1 <- get
  let current = current2 data1
      next = next2 data1
      emap = extraMap2 data1
      extraV = extraVert2 data1
      extraN = extraNorm2 data1
      extraT = extraText2 data1
      vert = verts ! pos
      text = texts ! pos
  maybe (do
        VM.write normacc pos (Just norm)
        modify $ \data2 -> data2{extraMap2=Map.insert (vert,text,norm) (fromIntegral pos) emap}
    )
    (const $ maybe (do
        VSM.write index (current+offset) (fromIntegral next)
        modify $ \data2 -> data2{next2=next + 1,
          extraVert2=extraV Seq.|> vert,
          extraNorm2=extraN Seq.|> norm,
          extraText2=extraT Seq.|> text,
          extraMap2=Map.insert (vert,text,norm) (fromIntegral next) emap}
      )
      (\i->VSM.write index (current+offset) (fromIntegral i))
      mextra
    )
    currentNorm
