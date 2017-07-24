--------------------------------------------------------------------------------
-- |
-- Module      :  EasyGL.Obj.Obj2IM
-- Copyright   :  Copyright (c) 2017, Jose Daniel Duran Toro
-- License     :  BSD3
--
-- Maintainer  :  Jose Daniel Duran Toro <jose_daniel_d@hotmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Functions required to transform a Obj to an EasyGL Indexed Model.
--
--------------------------------------------------------------------------------

module EasyGL.Obj.Obj2IM (
toIndexedModel,
groupToIndexedModel
  )
where

import           Control.Monad.Reader
import           Control.Monad.ST
import           Control.Monad.State.Lazy
import           Data.Foldable                (toList)
import qualified Data.Map.Strict              as Map
import qualified Data.Maybe                   as M
import qualified Data.Sequence                as Seq
import qualified Data.Vector                  as V
import qualified Data.Vector.Mutable          as VM
import qualified Data.Vector.Storable         as VS
import qualified EasyGL.IndexedModel          as IM
import           EasyGL.Obj.ObjData
import           Graphics.Rendering.OpenGL    hiding (get)

defaultNormal :: Maybe (Vector3 GLfloat) -> Vector3 GLfloat
defaultNormal (Just x) = x
defaultNormal Nothing  = Vector3 0 0 0

defaultTexture :: Maybe (Vector2 GLfloat) -> Vector2 GLfloat
defaultTexture (Just x) = x
defaultTexture Nothing  = Vector2 0 0

-- | A needed correction to all textureCoord, as image loading library loads everything upside down, and it is easier to correct here.
easyGLVector2Correction :: Vector2 GLfloat -> Vector2 GLfloat
easyGLVector2Correction (Vector2 x y) = Vector2 x (1-y)

toIndexedModel :: Obj -> [IM.IndexedModel]
toIndexedModel = map groupToIndexedModel . groups

groupToIndexedModel :: Group -> IM.IndexedModel
groupToIndexedModel g
    | M.isNothing norm0 && M.isNothing tex0 = IM.IndexedModel (V.convert vert0) VS.empty VS.empty (VS.fromList index0)
    | M.isJust norm0 && M.isNothing tex0 = runST $ do
      (newVert,newNorms,newIndex) <- rearrangeCaller defaultNormal vert0 (V.fromList index0) (V.fromList . toList . M.fromJust $ norm0) (V.fromList normIndex0)
      return $ IM.IndexedModel (V.convert newVert) (V.convert newNorms) VS.empty (V.convert newIndex)
    | M.isNothing norm0 && M.isJust tex0 = runST $ do
      (newVert,newText,newIndex) <- rearrangeCaller defaultTexture vert0 (V.fromList index0) (V.fromList . toList . M.fromJust $ tex0) (V.fromList textIndex0)
      return $ IM.IndexedModel (V.convert newVert) VS.empty (V.convert . V.map easyGLVector2Correction $ newText) (V.convert newIndex)
    | M.isJust norm0 && M.isJust tex0 = runST $ do
      (newVert,newNorms,newIndex) <- rearrangeCaller defaultNormal vert0 (V.fromList index0) (V.fromList . toList . M.fromJust $ norm0) (V.fromList normIndex0)
      let newThing = V.zip newVert newNorms
      (newThing,newText,newIndex) <- rearrangeCaller defaultTexture newThing newIndex (V.fromList . toList . M.fromJust $ tex0) (V.fromList textIndex0)
      let (newVert,newNorms) = V.unzip newThing
      return $ IM.IndexedModel (V.convert newVert) (V.convert newNorms) (V.convert . V.map easyGLVector2Correction $ newText) (V.convert newIndex)
    | otherwise = IM.emptyIndexedModel
  where
    vert0 = V.fromList . toList $ vertices g
    norm0 = normals g
    tex0 = textureCoord g
    (index0,textIndex0,normIndex0) = allIndexes g

rearrangeCaller :: (Ord a,Ord b,Integral c,Num c) => (Maybe b -> b) -> V.Vector a -> V.Vector c -> V.Vector b -> V.Vector c -> ST s (V.Vector a,V.Vector b,V.Vector c)
rearrangeCaller f vectorA indexA vectorB indexB = do
  mutableIndex <- V.thaw indexA
  acc <- VM.replicate (V.length vectorA) Nothing
  let reader = RearrangeRead vectorA vectorB acc mutableIndex indexB
  (RearrangeState _ extraA extraB _) <- runReaderT (execStateT rearrange defaultRearrangeState) reader
  freezed <- V.freeze acc
  let returnA = vectorA V.++ (V.fromList . toList $ extraA)
      returnB = V.map f freezed V.++ (V.fromList . toList $ extraB)
  returnC <- V.freeze mutableIndex
  return (returnA,returnB,returnC)

data RearrangeState a b c = RearrangeState {
  current :: Int,
  extraA  :: Seq.Seq a,
  extraB  :: Seq.Seq b,
  locator :: Map.Map (a,b) c
}

defaultRearrangeState :: RearrangeState a b c
defaultRearrangeState = RearrangeState 0 Seq.empty Seq.empty Map.empty

data RearrangeRead a b c s = RearrangeRead {
  readVetorA  :: V.Vector a,
  readVetorB  :: V.Vector b,
  resulVector :: VM.MVector s (Maybe b),
  indexA      :: VM.MVector s c,
  indexB      :: V.Vector c
}

type RearrangeM a b c s = StateT (RearrangeState a b c) (ReaderT (RearrangeRead a b c s) (ST s)) ()

rearrange :: (Ord a,Ord b,Integral c,Num c) => RearrangeM a b c s
rearrange = do
  readdata <- ask
  state <- get
  unless (current state == VM.length (indexA readdata)) $ do
    currentIndexA <- VM.read (indexA readdata) (fromIntegral $ current state)
    let currentIndexB = indexB readdata V.! fromIntegral (current state)
        currentA = readVetorA readdata V.! fromIntegral currentIndexA
        currentB = readVetorB readdata V.! fromIntegral currentIndexB
    currentMaybeB <- VM.read (resulVector readdata) (fromIntegral currentIndexA)
    if M.isNothing currentMaybeB then
      VM.write (resulVector readdata) (fromIntegral currentIndexA) (Just currentB)
    else
      unless (currentB == M.fromJust currentMaybeB) $ do
        let search = Map.lookup (currentA,currentB) $ locator state
        case search of
          Just x -> VM.write (indexA readdata) (fromIntegral $ current state) x
          Nothing -> do
            let newIndex = fromIntegral $ V.length (readVetorA readdata) + Seq.length (extraA state)
            VM.write (indexA readdata) (fromIntegral $ current state) newIndex
            modify' (\s-> s{
                  extraA=extraA s Seq.|> currentA,
                  extraB=extraB s Seq.|> currentB,
                  locator=Map.insert (currentA,currentB) newIndex (locator s)
                }
              )
    modify' (\s->s{current=current state + 1})
    rearrange
