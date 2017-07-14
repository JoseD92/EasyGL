--------------------------------------------------------------------------------
-- |
-- Module      :  EasyGL.Util
-- Copyright   :  Copyright (c) 2017, Jose Daniel Duran Toro
-- License     :  BSD3
--
-- Maintainer  :  Jose Daniel Duran Toro <jose_daniel_d@hotmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Math functions for Graphics.Rendering.OpenGL types.
--
--------------------------------------------------------------------------------

module EasyGL.Util (
  normVec2,
  normalizeVec2,
  dotVec2,
  normVec3,
  normalizeVec3,
  dotVec3,
  crossVec3,
  normVec4,
  normalizeVec4,
  dotVec4)
where

import qualified Graphics.Rendering.OpenGL as GL

normVec2 :: (Floating a) => GL.Vector2 a -> a
normVec2 (GL.Vector2 x y) = sqrt $ x**2 + y**2

normalizeVec2 :: (Floating a) => GL.Vector2 a -> GL.Vector2 a
normalizeVec2 v@(GL.Vector2 x y) = GL.Vector2 (x/norm) (y/norm)
  where
    norm = normVec2 v

dotVec2 :: (Num a) => GL.Vector2 a -> GL.Vector2 a -> a
dotVec2 (GL.Vector2 x1 y1) (GL.Vector2 x2 y2) = x1*x2 + y1*y2

normVec3 :: (Floating a) => GL.Vector3 a -> a
normVec3 (GL.Vector3 x y z) = sqrt $ x**2 + y**2 + z**2

normalizeVec3 :: (Floating a) => GL.Vector3 a -> GL.Vector3 a
normalizeVec3 v@(GL.Vector3 x y z) = GL.Vector3 (x/norm) (y/norm) (z/norm)
  where
    norm = normVec3 v

dotVec3 :: (Num a) => GL.Vector3 a -> GL.Vector3 a -> a
dotVec3 (GL.Vector3 x1 y1 z1) (GL.Vector3 x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

crossVec3 :: (Num a) => GL.Vector3 a -> GL.Vector3 a -> GL.Vector3 a
crossVec3 (GL.Vector3 x1 y1 z1) (GL.Vector3 x2 y2 z2) =
  GL.Vector3 (y1*z2-z1*y2) (z1*x2-x1*z2) (x1*y2-y1*x2)

normVec4 :: (Floating a) => GL.Vector4 a -> a
normVec4 (GL.Vector4 x y z w) = sqrt $ x**2 + y**2 + z**2 + w**2

normalizeVec4 :: (Floating a) => GL.Vector4 a -> GL.Vector4 a
normalizeVec4 v@(GL.Vector4 x y z w) = GL.Vector4 (x/norm) (y/norm) (z/norm) (w/norm)
  where
    norm = normVec4 v

dotVec4 :: (Num a) => GL.Vector4 a -> GL.Vector4 a -> a
dotVec4 (GL.Vector4 x1 y1 z1 w1) (GL.Vector4 x2 y2 z2 w2) = x1*x2 + y1*y2 + z1*z2 + w1*w2

instance (Num a) => Num (GL.Vector2 a) where
  (+) (GL.Vector2 x1 y1) (GL.Vector2 x2 y2) = GL.Vector2 (x1+x2) (y1+y2)
  (-) (GL.Vector2 x1 y1) (GL.Vector2 x2 y2) = GL.Vector2 (x1-x2) (y1-y2)
  (*) (GL.Vector2 x1 y1) (GL.Vector2 x2 y2) = GL.Vector2 (x1*x2) (y1*y2) --This makes no sense, do not use.
  negate (GL.Vector2 x y) = GL.Vector2 (negate x) (negate y)
  abs (GL.Vector2 x y) = GL.Vector2 (abs x) (abs y)
  signum (GL.Vector2 x y) = GL.Vector2 (signum x) (signum y)
  fromInteger x = GL.Vector2 (fromInteger x) (fromInteger x)

instance (Num a) => Num (GL.Vector3 a) where
  (+) (GL.Vector3 x1 y1 z1) (GL.Vector3 x2 y2 z2) = GL.Vector3 (x1+x2) (y1+y2) (z1+z2)
  (-) (GL.Vector3 x1 y1 z1) (GL.Vector3 x2 y2 z2) = GL.Vector3 (x1-x2) (y1-y2) (z1-z2)
  (*) = crossVec3
  negate (GL.Vector3 x y z) = GL.Vector3 (negate x) (negate y) (negate z)
  abs (GL.Vector3 x y z) = GL.Vector3 (abs x) (abs y) (abs z)
  signum (GL.Vector3 x y z) = GL.Vector3 (signum x) (signum y) (signum z)
  fromInteger x = GL.Vector3 (fromInteger x) (fromInteger x) (fromInteger x)
