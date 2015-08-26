module WebGL where

import Control.Monad.Reader

import qualified GHCJS.Types    as T
import qualified GHCJS.Foreign  as F
import qualified GHCJS.Marshal  as M

{-
FFI for WebGL, glMatrix, and just enough DOM to get a demo canvas and animation
-}

data Canvas_
type Canvas = T.JSObject Canvas_
data GL_
type GL = T.JSObject GL_
data Shader_ a
type Shader a = T.JSObject (Shader_ a)
data VertexShader
data FragmentShader
data ShaderProgram_
type ShaderProgram = T.JSObject ShaderProgram_
data ShaderAttribute_
type ShaderAttribute = T.JSObject ShaderAttribute_
data ShaderUniform_
type ShaderUniform = T.JSObject ShaderUniform_
data Buffer_
type Buffer = T.JSObject Buffer_
data BufferBinding_
type BufferBinding = T.JSObject BufferBinding_
data DrawType_
type DrawType = T.JSObject DrawType_

data Mat4_
type Mat4 = T.JSObject Mat4_

foreign import javascript unsafe "document.getElementById($1)" js_documentGetElementById :: T.JSString -> IO (T.JSRef a)
foreign import javascript unsafe "$1.getContext('experimental-webgl')" js_getContext :: Canvas -> IO GL
foreign import javascript unsafe "$1.width" js_getCanvasWidth :: Canvas -> IO Int
foreign import javascript unsafe "$1.height" js_getCanvasHeight :: Canvas -> IO Int

-- from http://weblog.luite.com/wordpress/?p=127
foreign import javascript unsafe
  "var req = window.requestAnimationFrame ||\
  \           window.mozRequestAnimationFrame ||\
  \           window.webkitRequestAnimationFrame ||\
  \           window.msRequestAnimationFrame;\
  \ var f = function() { $1(); req(f); };\
  \ req(f);"
  animate :: T.JSFun (IO ()) -> IO ()

foreign import javascript unsafe "Date.now()" js_now :: IO Double

foreign import javascript unsafe "$1.clearColor($2, $3, $4, $5)" 
        js_glClearColor :: GL -> Float -> Float -> Float -> Float -> IO ()

foreign import javascript unsafe "$1.enable($1.DEPTH_TEST)" js_glEnableDepthTest :: GL -> IO ()

foreign import javascript unsafe "$1.createShader($1.FRAGMENT_SHADER)" js_glCreateFragmentShader :: GL -> IO (Shader FragmentShader)

foreign import javascript unsafe "$1.createShader($1.VERTEX_SHADER)" js_glCreateVertexShader :: GL -> IO (Shader VertexShader)

foreign import javascript unsafe "$1.shaderSource($2,$3)" js_glShaderSource :: GL -> Shader a -> T.JSString -> IO (Shader a)

foreign import javascript unsafe "$1.compileShader($2)" js_glCompileShader :: GL -> Shader a -> IO (Shader a)

foreign import javascript unsafe "$1.createProgram()" js_glCreateProgram :: GL -> IO ShaderProgram

foreign import javascript unsafe "$1.attachShader($2,$3)" js_glAttachShader :: GL -> ShaderProgram -> (Shader a) -> IO ()

foreign import javascript unsafe "$1.linkProgram($2)" js_glLinkProgram :: GL -> ShaderProgram -> IO ()

foreign import javascript unsafe "$1.useProgram($2)" js_glUseProgram :: GL -> ShaderProgram -> IO ()

foreign import javascript unsafe "$1.enableVertexAttribArray($2)" js_glEnableVAA :: GL -> ShaderAttribute -> IO ()

foreign import javascript unsafe "$1.getAttribLocation($2,$3)" 
  js_glGetAttribLocation :: GL -> ShaderProgram -> T.JSString -> IO ShaderAttribute

foreign import javascript unsafe "$1.getUniformLocation($2,$3)" 
  js_glGetUniformLocation :: GL -> ShaderProgram -> T.JSString -> IO ShaderUniform

-- The second parameters to these change the result type...
foreign import javascript unsafe "$1.getShaderParameter($2,$1.COMPILE_STATUS)" 
  js_glGetShaderCompileStatus :: GL -> (Shader a) -> IO T.JSBool

foreign import javascript unsafe "$1.getProgramParameter($2,$1.LINK_STATUS)" 
  js_glGetProgramLinkStatus :: GL -> ShaderProgram -> IO T.JSBool

foreign import javascript unsafe "$1.getShaderInfoLog($2)" js_glGetShaderInfoLog :: GL -> Shader a -> IO T.JSString

foreign import javascript "mat4.create()" js_mat4Create :: IO Mat4

foreign import javascript "$1.uniformMatrix4fv($2, false, $3)" glUniformMatrix4fv :: GL -> ShaderUniform -> Mat4 -> IO ()

foreign import javascript "$1.createBuffer()" js_glCreateBuffer :: GL -> IO Buffer

foreign import javascript "$1.bindBuffer($1[$2], $3)" js_glBindBuffer :: GL -> BufferBinding -> Buffer -> IO ()

foreign import javascript "'ARRAY_BUFFER'" arrayBuffer :: BufferBinding

-- For now, assume we're only buffering floats, with original... byte order? I guess?
-- and gl.STATIC_DRAW is a performance hint. We'll leave it for now.
-- BufferBinding done this way isn't quite a hack.
foreign import javascript "$1.bufferData($1[$2], new Float32Array($3), $1.STATIC_DRAW)" 
        js_glBufferData :: GL -> BufferBinding -> T.JSRef [Float] -> IO ()

-- Note: We're using a very old version of glMatrix. Docs at https://github.com/toji/gl-matrix/blob/4cbb9339ee074a7ad7e65fa7b40ca279d5253eef/gl-matrix.js are closer than the current...
foreign import javascript "mat4.perspective($1, $2, $3, $4, $5)"
  js_mat4perspective :: Float -> Float -> Float -> Float -> Mat4 -> IO ()

foreign import javascript "mat4.identity($1)" js_mat4identity :: Mat4 -> IO ()

foreign import javascript "mat4.translate($1, $2)" js_mat4translate :: Mat4 -> T.JSRef [Float] -> IO ()

foreign import javascript "mat4.set($1, $2)" js_mat4set :: Mat4 -> Mat4 -> IO ()

foreign import javascript "mat4.rotate($1, $2, $3)" js_mat4rotate :: Mat4 -> Double -> T.JSRef [Float] -> IO () 

-- gl.FLOAT should get sorted out with buffer types...
foreign import javascript "$1.vertexAttribPointer($2, $3, $1.FLOAT, false, 0, 0)"
  js_glVertexAttribPointer :: GL -> ShaderAttribute -> Int -> IO ()

foreign import javascript "$1.drawArrays($1[$2], $3, $4)" js_glDrawArrays :: GL -> DrawType -> Int -> Int -> IO ()

foreign import javascript "'TRIANGLES'" drawTriangles :: DrawType
foreign import javascript "'TRIANGLE_STRIP'" drawTriangleStrip :: DrawType

foreign import javascript "$1.clear($1.COLOR_BUFFER_BIT | $1.DEPTH_BUFFER_BIT)" js_glClear :: GL -> IO ()

foreign import javascript "$1.viewport($2, $3, $4, $5)" js_glViewport :: GL -> Int -> Int -> Int -> Int -> IO ()

