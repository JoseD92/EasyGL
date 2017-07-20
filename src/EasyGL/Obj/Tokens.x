{
module EasyGL.Obj.Tokens where
import Graphics.Rendering.OpenGL
}

%wrapper "posn"
$graphic = [\.\;\,\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^\/\\\ 0-9a-zA-Z]

@entero10 = [0-9][0-9]*
@float1 = [\-]? @entero10 "." @entero10 [eE] [\-\+] @entero10 | @entero10 [eE] [\-\+] @entero10
@float2 = [\-]? @entero10 "." @entero10

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
@usemtl{ \(AlexPn _ l c) s -> Usemtl (unwords.tail.words $ s)
}
@mtllib{ \(AlexPn _ l c) s -> Mtllib (unwords.tail.words $ s)
}
@shadingoff{ \(AlexPn _ l c) s -> ShadingOff
}
@shading{ \(AlexPn _ l c) s -> Shading (read.(drop 2) $ s)
}

@groupVertex{ \(AlexPn _ l c) s -> GroupV (unwords.tail.words.head.lines $ s)
}
@group{ \(AlexPn _ l c) s -> Group (unwords.tail.words $ s)
}
@object{ \(AlexPn _ l c) s -> Object (unwords.tail.words $ s)
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

@float1 { \(AlexPn _ l c) s -> TFloat (read s)
}

@float2 { \(AlexPn _ l c) s -> TFloat (read s)
}

@entero10 { \(AlexPn _ l c) s -> TInt (read s)
} -- numeros base 10


{

data Token = Vertex | VertexTexture | VertexNormal | Face | Group String | GroupV String | Object String | TDiv |
  TFloat GLfloat | TInt GLuint | Usemtl String | Mtllib String | Shading Int | ShadingOff deriving (Show)

}
