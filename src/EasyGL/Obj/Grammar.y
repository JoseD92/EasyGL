
{

module EasyGL.Obj.Grammar where
import qualified EasyGL.Obj.Tokens as L
import EasyGL.Obj.ObjData
import Graphics.Rendering.OpenGL
import Data.Sequence
import Data.Foldable (toList)
}

--%monad { StateT ParseState IO } { (>>=) } { return }
%name parseObj All
%tokentype { L.Token }
%error { parseError }

%token
    Object {L.Object $$}
    Group {L.Group $$}
    Vertex {L.Vertex}
    VertexTexture {L.VertexTexture}
    VertexNormal {L.VertexNormal}
    Face {L.Face}
    TFloat {L.TFloat $$}
    TInt {L.TInt $$}
    TUInt {L.TUInt $$}
    '/' { L.TDiv }
    Usemtl {L.Usemtl $$}
    Mtllib {L.Mtllib $$}
    Shadingoff {L.ShadingOff}
    Shading{L.Shading $$}
    GroupV{L.GroupV $$}

%%

All : Mtllib Groups { Obj Nothing (Just $1) (toList $2) }
  | Groups { Obj Nothing Nothing (toList $1) }

toFloat : TFloat { $1 }
  | TInt { fromIntegral $1 :: GLfloat}
  | TUInt { fromIntegral $1 :: GLfloat}


Groups : GroupBlock { singleton $1 }
  | Groups GroupBlock { $1 |> $2 }


GroupBlock : Object Geometry Faces { (\(v,tc,n) -> Group (Just $1) v tc n $3 ) $2 }
  | GroupV toFloat toFloat toFloat Geometry Faces { (\(v,tc,n) -> Group (Just $1) ((Vertex3 $2 $3 $4) <| v) tc n $6 ) $5 }
  | Geometry Faces { (\(v,tc,n) -> Group Nothing v tc n $2 ) $1 }


Faces : FaceLineBlock { singleton (IndexBlock Nothing Nothing Nothing $1) }
  | FaceBlock { $1 }


FaceBlock : FaceBlockHeader FaceLineBlock { singleton ((\(name,shad,mat) -> IndexBlock name shad mat $2 ) $1) }
  | FaceBlock FaceBlockHeader FaceLineBlock { $1 |> ((\(name,shad,mat) -> IndexBlock name shad mat $3 ) $2) }


FaceBlockHeader : ShadingLine { (Nothing,$1,Nothing) }
  | Usemtl { (Nothing,Nothing,Just $1) }
  | Group { (Just $1,Nothing,Nothing) }
  | Group ShadingLine { (Just $1,$2,Nothing) }
  | Group Usemtl { (Just $1,Nothing,Just $2) }
  | ShadingLine Group { (Just $2,$1,Nothing) }
  | Usemtl Group { (Just $2,Nothing,Just $1) }
  | ShadingLine Usemtl { (Nothing,$1,Just $2) }
  | Usemtl ShadingLine { (Nothing,$2,Just $1) }
  | Group ShadingLine Usemtl { (Just $1,$2,Just $3) }
  | Group Usemtl ShadingLine { (Just $1,$3,Just $2) }
  | ShadingLine Group Usemtl { (Just $2,$1,Just $3) }
  | Usemtl Group ShadingLine { (Just $2,$3,Just $1) }
  | ShadingLine Usemtl Group { (Just $3,$1,Just $2) }
  | Usemtl ShadingLine Group { (Just $3,$2,Just $1) }


Geometry : VertexBlock { ($1,Nothing,Nothing) }
  | VertexBlock VertexTextureBlock { ($1,Just $2,Nothing) }
  | VertexBlock VertexNormalBlock { ($1,Nothing,Just $2) }
  | VertexBlock VertexTextureBlock VertexNormalBlock { ($1,Just $2,Just $3) }
  | VertexBlock VertexNormalBlock VertexTextureBlock { ($1,Just $3,Just $2) }


ShadingLine : Shadingoff { Nothing }
  | Shading { Just $1 }


VertexLine : Vertex toFloat toFloat toFloat { Vertex3 $2 $3 $4 }
  | Vertex toFloat toFloat toFloat toFloat { Vertex3 $2 $3 $4 } -- some implementations use an optional w (x,y,z[,w]), we ignore it.

VertexBlock : VertexLine { singleton $1 }
  | VertexBlock VertexLine { $1 |> $2 }


VertexTextureLine : VertexTexture toFloat toFloat { Vector2 $2 $3 }
  | VertexTexture toFloat toFloat toFloat { Vector2 $2 $3 } -- some implementations use an optional w (u, v [,w]), we ignore it.

VertexTextureBlock : VertexTextureLine { singleton $1 }
  | VertexTextureBlock VertexTextureLine { $1 |> $2 }


VertexNormalLine : VertexNormal toFloat toFloat toFloat { Vector3 $2 $3 $4 }

VertexNormalBlock : VertexNormalLine { singleton $1 }
  | VertexNormalBlock VertexNormalLine { $1 |> $2 }


FaceNode : TUInt { FaceNode ($1-1) Nothing Nothing }
  | TUInt '/' TUInt { FaceNode ($1-1) (Just ($3-1)) Nothing }
  | TUInt '/' TUInt '/' TUInt { FaceNode ($1-1) (Just ($3-1)) (Just ($5-1)) }
  | TUInt '/' '/' TUInt  { FaceNode ($1-1) Nothing (Just ($4-1)) }

FaceLine : Face FaceNodeList { $2 }

FaceNodeList : FaceNode { singleton $1 }
  | FaceNodeList FaceNode { $1 |> $2 }

FaceLineBlock : FaceLine { singleton $1 }
  | FaceLineBlock FaceLine { $1 |> $2 }

{

--errStrPut = hPutStrLn stderr

--printError = lift.errStrPut

--sprint = lift.putStrLn

parseError :: [L.Token] -> b
parseError t = error ("Parse error: " ++ show t)

}
