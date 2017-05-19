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
