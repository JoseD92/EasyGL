--------------------------------------------------------------------------------
-- |
-- Module      :  EasyGL.Obj.ObjData
-- Copyright   :  Copyright (c) 2017, Jose Daniel Duran Toro
-- License     :  BSD3
--
-- Maintainer  :  Jose Daniel Duran Toro <jose_daniel_d@hotmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Data types for storing a parsed Obj file.
--
--------------------------------------------------------------------------------

module EasyGL.Obj.ObjData (Obj(..),Group(..),emptyGroup,emptyObj)
where

import Graphics.Rendering.OpenGL

data Group = Group {
  groupName :: !String,
  vertices :: ![Vertex3 GLfloat],
  indexes :: ![GLuint],
  normals :: ![Vector3 GLfloat],
  normalsIndex :: ![GLuint],
  textureCoord :: ![Vector2 GLfloat],
  textureCoordIndex :: ![GLuint]
} deriving (Show)

data Obj = Obj {
  objName :: String,
  groups :: [Group]
} deriving (Show)

emptyGroup :: String -> Group
emptyGroup s = Group s [] [] [] [] [] []

emptyObj :: String -> Obj
emptyObj s = Obj s []
