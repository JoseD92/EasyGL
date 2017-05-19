module EasyGL.IndexedModel (
  IndexedModel(..),
  emptyIndexedModel,
  renderNormals,
  generateNormalsSoft,
  generateNormalsHard
)
where

import Graphics.Rendering.OpenGL
import Data.Array
import EasyGL.Util
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import Data.Foldable (toList)

data IndexedModel = IndexedModel {
  vertices :: ![Vertex3 GLfloat],
  normals :: ![Vector3 GLfloat],
  textureCoord :: ![Vector2 GLfloat],
  indexes :: ![GLuint]
} deriving (Show)

emptyIndexedModel :: IndexedModel
emptyIndexedModel = IndexedModel [] [] [] []

toVector :: (Num a) => Vertex3 a -> Vertex3 a -> Vector3 a
toVector (Vertex3 x1 y1 z1)  (Vertex3 x2 y2 z2) = Vector3 (x1-x2) (y1-y2) (z1-z2)

toVertex :: (Num a) => Vertex3 a -> Vector3 a -> Vertex3 a
toVertex (Vertex3 x1 y1 z1)  (Vector3 x2 y2 z2) = Vertex3 (x1+x2) (y1+y2) (z1+z2)

renderLine :: (Vertex3 GLfloat,Vertex3 GLfloat) -> IO ()
renderLine (a,b) = do
  vertex a
  vertex b

renderNormals :: IndexedModel -> IO ()
renderNormals g = renderPrimitive Lines $ mapM_ renderLine $ zip verts newVerts
  where
    verts = vertices g
    newVerts = zipWith toVertex verts (normals g)


--Normals generation
generateNormalsSoft :: IndexedModel -> IndexedModel
generateNormalsSoft g = g{normals=newNormals}
  where
    len = fromIntegral $ length verts
    verts = vertices g
    index = indexes g
    newNormals = generateNormalsSoftAux index (listArray (0,len-1) verts) (listArray (0,len-1) (repeat Nothing))

(<+>) :: (Num a) => Maybe a -> a -> a
Nothing <+> e = 0 + e
(Just a) <+> e = a+e

--some vertex may not be use be the index array, so it is important not to normalize a vector 0 0 0, that is a nan error
--that is the reason for normalsAcc to be a Array GLuint (Maybe (Vector3 GLfloat)) and not a Array GLuint (Vector3 GLfloat)
--in the second case the array would have need to be initialize as (listArray (0,len-1) (repeat Vector3 0 0 0)), leading to nan
generateNormalsSoftAux :: [GLuint] -> Array GLuint (Vertex3 GLfloat) -> Array GLuint (Maybe (Vector3 GLfloat)) -> [Vector3 GLfloat]
generateNormalsSoftAux [] _ n = map maybeNormalize $ elems n
  where
    maybeNormalize Nothing = Vector3 0 0 0
    maybeNormalize (Just v) = normalizeVec3 v
generateNormalsSoftAux (x:y:z:xs) verts normalsAcc = generateNormalsSoftAux xs verts updatedNormals
  where
    v1 = toVector (verts ! y) (verts ! x)
    v2 = toVector (verts ! z) (verts ! x)
    normal = normalizeVec3 $ crossVec3 v1 v2
    updatedNormals = normalsAcc // [(x,Just ((normalsAcc ! x)<+>normal)),(y,Just ((normalsAcc ! y)<+>normal)),(z,Just ((normalsAcc ! z)<+>normal))]
generateNormalsSoftAux _ _ _ = []

type EVert = Either (Vertex3 GLfloat) (Vertex3 GLfloat,Vector2 GLfloat)

mylefts :: [EVert] -> [Vertex3 GLfloat]
mylefts [] = []
mylefts (Left x:xs) = x:mylefts xs
mylefts (_:xs) = mylefts xs

myrights :: [EVert] -> [(Vertex3 GLfloat,Vector2 GLfloat)]
myrights [] = []
myrights (Right x:xs) = x:myrights xs
myrights (_:xs) = myrights xs

generateNormalsHard :: IndexedModel -> IndexedModel
generateNormalsHard g = IndexedModel nv nn nt ni
  where
    len = fromIntegral $ length verts
    len2 = fromIntegral $ (length $ indexes g)
    verts = vertices g
    index = listArray (0,len2-1) $ indexes g
    mydata = if null (textureCoord g) then listArray (0,len-1) (map Left verts) else listArray (0,len-1) $ map Right $ zip verts (textureCoord g)
    (edata,nn,ni) = generateNormalsHardAux 0 len2 len index mydata (listArray (0,len-1) (repeat Nothing)) Seq.empty Seq.empty Map.empty
    (nv,nt) = if null (textureCoord g) then (mylefts edata,[]) else unzip $ myrights edata

norm :: Vertex3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat -> Vector3 GLfloat
norm x y z = normalizeVec3 $ crossVec3 v1 v2
  where
    v1 = toVector y x
    v2 = toVector z x

enorm :: EVert -> EVert -> EVert -> Vector3 GLfloat
enorm (Left x) (Left y) (Left z) = norm x y z
enorm (Right (x,_)) (Right (y,_)) (Right (z,_)) = norm x y z
enorm _ _ _ = Vector3 0 0 0

generateNormalsHardAux :: GLuint -> GLuint -> GLuint -> Array GLuint GLuint -> Array GLuint EVert -> Array GLuint (Maybe (Vector3 GLfloat)) -> Seq.Seq EVert -> Seq.Seq (Vector3 GLfloat) -> Map.Map (EVert,Vector3 GLfloat) GLuint -> ([EVert],[Vector3 GLfloat],[GLuint])
generateNormalsHardAux current limit next index edata mnorm extraA extraB extraMap
  | current == limit = (elems edata ++ toList extraA,map toVer (elems mnorm) ++ toList extraB,elems index)
  | otherwise = generateNormalsHardAux (current+3) limit new_next new_index edata new_mnorm new_extraA new_extraB new_extraMap
  where
    toVer Nothing = Vector3 0 0 0
    toVer (Just x) = x
    currentData1 = edata ! (index ! current)
    currentData2 = edata ! (index ! (current+1))
    currentData3 = edata ! (index ! (current+2))
    currentNorm1 = mnorm ! (index ! current)
    currentNorm2 = mnorm ! (index ! (current+1))
    currentNorm3 = mnorm ! (index ! (current+2))
    norm = enorm currentData1 currentData2 currentData3
    mextra1 = Map.lookup (currentData1,norm) extraMap
    mextra2 = Map.lookup (currentData2,norm) extraMap
    mextra3 = Map.lookup (currentData3,norm) extraMap
    (next1,index1,norm1,extraA1,extraB1,extraMap1) = maybe
      (next,index,mnorm//[(index ! current,Just norm)],extraA,extraB,extraMap)
      (const $ maybe
        (next+1,index//[(current,next)],mnorm,extraA Seq.|> currentData1,extraB Seq.|> norm,Map.insert (currentData1,norm) next extraMap)
        (\i->(next,index//[(current,i)],mnorm,extraA,extraB,extraMap))
        mextra1
      )
      currentNorm1
    (next2,index2,norm2,extraA2,extraB2,extraMap2) = maybe
      (next1,index1,norm1//[(index ! (current+1),Just norm)],extraA1,extraB1,extraMap1)
      (const $ maybe
        (next1+1,index1//[(current+1,next1)],norm1,extraA1 Seq.|> currentData2,extraB1 Seq.|> norm,Map.insert (currentData2,norm) next1 extraMap1)
        (\i->(next1,index1//[(current+1,i)],norm1,extraA1,extraB1,extraMap1))
        mextra2
      )
      currentNorm2
    (new_next,new_index,new_mnorm,new_extraA,new_extraB,new_extraMap) = maybe
      (next2,index2,norm2//[(index ! (current+2),Just norm)],extraA2,extraB2,extraMap2)
      (const $ maybe
        (next2+1,index2//[(current+2,next2)],norm2,extraA2 Seq.|> currentData3,extraB2 Seq.|> norm,Map.insert (currentData3,norm) next2 extraMap2)
        (\i->(next2,index2//[(current+2,i)],norm2,extraA2,extraB2,extraMap2))
        mextra3
      )
      currentNorm3
