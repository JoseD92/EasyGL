module EasyGL.Obj.Obj (
  EasyGL.Obj.ObjData.Obj(..),
  EasyGL.Obj.ObjData.Group(..),
  readObj,
  toIndexedModel
  )
where

import EasyGL.Obj.Grammar
import EasyGL.Obj.Tokens
import EasyGL.Obj.ObjData
import EasyGL.Obj.Obj2IM

readObj :: String -> Obj
readObj = fixObj.parseObj.alexScanTokens

fixGroup :: Group -> Group
fixGroup g = g{vertices=reverse.vertices $ g,normals=reverse.normals $ g,
  textureCoord=reverse.textureCoord $ g}

fixObj :: Obj -> Obj
fixObj o = o{groups=map fixGroup (groups o)}
