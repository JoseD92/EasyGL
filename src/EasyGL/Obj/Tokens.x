{
module EasyGL.Obj.Tokens where
import Graphics.Rendering.OpenGL
import qualified Data.Maybe as M
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Data.ByteString.Conversion as BSC
}

%wrapper "posn-bytestring"
$graphic = [\.\;\,\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^\/\\\ 0-9a-zA-Z]

@entero10 = [0-9][0-9]*
@float1 = [\-]? @entero10 "." @entero10 [eE] [\-\+] @entero10 | @entero10 [eE] [\-\+] @entero10
@float2 = [\-]? @entero10 "." @entero10
@signedInteger = [\-]? @entero10

@vertex = "v "

@vertexTexture = "vt "

@vertexNormal = "vn "

@face = "f "

@object = "o " .*

@groupVertex = "g " .*  [\n] "v "

@group = "g " .*

@div = "/"

@usemtl = "usemtl " .*

@mtllib = "mtllib " .*

@shadingoff = "s off"

@shading = "s " @entero10

tokens :-

$white+ ; --espacios
"#".* ; --comentarios de resto de linea
@usemtl{ \(AlexPn _ l c) s -> Usemtl (BS8.unwords . tail . BS8.words $ s)
}
@mtllib{ \(AlexPn _ l c) s -> Mtllib (BS8.unwords . tail . BS8.words $ s)
}
@shadingoff{ \(AlexPn _ l c) s -> ShadingOff
}
@shading{ \(AlexPn _ l c) s -> Shading (M.fromJust . BSC.fromByteString' . BS.drop 2 $ s)
}

@groupVertex{ \(AlexPn _ l c) s -> GroupV (BS8.unwords . tail . BS8.words . head . BS8.lines $ s)
}
@group{ \(AlexPn _ l c) s -> Group (BS8.unwords . tail . BS8.words $ s)
}
@object{ \(AlexPn _ l c) s -> Object (BS8.unwords . tail . BS8.words $ s)
}

@div { \(AlexPn _ l c) s -> TDiv
}

@vertex { \(AlexPn _ l c) s -> Vertex
}

@vertexTexture { \(AlexPn _ l c) s -> VertexTexture
}

@vertexNormal { \(AlexPn _ l c) s -> VertexNormal
}

@face { \(AlexPn _ l c) s -> Face
}

@float1 { \(AlexPn _ l c) s -> TFloat (realToFrac (M.fromJust (BSC.fromByteString' s) :: Double))
}

@float2 { \(AlexPn _ l c) s -> TFloat (realToFrac (M.fromJust (BSC.fromByteString' s) :: Double))
}

@entero10 { \(AlexPn _ l c) s -> TUInt (M.fromJust $ BSC.fromByteString' s)
}

@signedInteger { \(AlexPn _ l c) s -> TInt (M.fromJust $ BSC.fromByteString' s)
}


{

data Token = Vertex | VertexTexture | VertexNormal | Face | Group BS.ByteString | GroupV BS.ByteString | Object BS.ByteString | TDiv |
  TFloat GLfloat | TUInt GLuint | TInt GLint | Usemtl BS.ByteString | Mtllib BS.ByteString | Shading Int | ShadingOff deriving (Show)

}
