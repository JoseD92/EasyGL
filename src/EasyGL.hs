--------------------------------------------------------------------------------
-- |
-- Module      :  EasyGL
-- Copyright   :  Copyright (c) 2017, Jose Daniel Duran Toro
-- License     :  BSD3
--
-- Maintainer  :  Jose Daniel Duran Toro <jose_daniel_d@hotmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- A module that makes OpenGL easier in haskell.
--
--------------------------------------------------------------------------------

module EasyGL (
  module EasyGL.Camera,
  module EasyGL.Entity,
  module EasyGL.IndexedModel,
  module EasyGL.Material,
  module EasyGL.Obj,
  module EasyGL.Shader,
  module EasyGL.Texture,
  module EasyGL.Util
) where

import           EasyGL.Camera
import           EasyGL.Entity
import           EasyGL.IndexedModel
import           EasyGL.Material
import           EasyGL.Obj
import           EasyGL.Shader
import           EasyGL.Texture
import           EasyGL.Util
