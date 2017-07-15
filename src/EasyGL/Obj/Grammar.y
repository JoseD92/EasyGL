
{

module EasyGL.Obj.Grammar where
import qualified EasyGL.Obj.Tokens as L
import EasyGL.Obj.ObjData
import Graphics.Rendering.OpenGL
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
    '/' { L.TDiv }


%%

All : Objeto { $1 }
  | Grupo { (emptyObj "") {groups=[$1]} }

Grupo : Grupo2 Caras { $1{indexes=iindices $2,normalsIndex=vindices $2,textureCoordIndex=tindices $2} }

Grupo2 : Group { emptyGroup $1 }
  | Grupo2 Vertex toFloat toFloat toFloat { $1{vertices=(Vertex3 $3 $4 $5):vertices $1} }
  | Grupo2 VertexTexture toFloat toFloat { $1{textureCoord=(Vector2 $3 $4):textureCoord $1} }
  | Grupo2 VertexNormal toFloat toFloat toFloat { $1{normals=(Vector3 $3 $4 $5):normals $1} }

Caras : Cara { [$1] }
  | Caras Cara { $2:$1 }

Cara : Face TInt TInt TInt { ([$2-1,$3-1,$4-1],[],[]) }
   | Face TInt '/' TInt TInt '/' TInt TInt '/' TInt { ([$2-1,$5-1,$8-1],[],[$4-1,$7-1,$10-1]) }
   | Face TInt '/' TInt '/' TInt TInt '/' TInt '/' TInt TInt '/' TInt '/' TInt { ([$2-1,$7-1,$12-1],[$6-1,$11-1,$16-1],[$4-1,$9-1,$14-1]) }
   | Face TInt '/' '/' TInt TInt '/' '/' TInt TInt '/' '/' TInt { ([$2-1,$6-1,$10-1],[$5-1,$9-1,$13-1],[]) }

Objeto : Object { emptyObj $1 }
  | Objeto Grupo { $1{groups=$2:groups $1} }

toFloat : TFloat { $1 }
  | TInt { fromIntegral $1 :: GLfloat}


{

--errStrPut = hPutStrLn stderr

--printError = lift.errStrPut

--sprint = lift.putStrLn

iindices l = foldr help [] l
  where
    help (x,_,_) acc = x++acc

vindices l = foldr help [] l
  where
    help (_,x,_) acc = x++acc

tindices l = foldr help [] l
  where
    help (_,_,x) acc = x++acc

parseError :: [L.Token] -> b
parseError t = error ("Parse error: " ++ show t)

}
