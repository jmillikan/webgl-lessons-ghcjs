{-# LANGUAGE JavaScriptFFI      #-}
{-# LANGUAGE OverloadedStrings  #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Reader

import qualified GHCJS.Types    as T
import qualified GHCJS.Foreign  as F
import qualified GHCJS.Marshal  as M

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

{-
After importing our foreign calls, we use gl0 to gl6 to lift most of them into GLIO a -
with GL as context. This includes glMatrix functions, which are IO anyway because of the
tendency to update destructively.
-}

type GLIO a = ReaderT GL IO a

gl0 f = ReaderT f

gl1 f x = ReaderT $ flip f x

-- Non-haskeller notes: gl0 .. gl4 will take a 1..5 arg function with GL as
-- first argument and give you back a function that runs in a GL 'context'
-- flip and rot are details - I put GL at the 'wrong end' of all the foreign calls.

gl2 f a b = ReaderT $ rot f a b
  where rot furn a b gl = furn gl a b

gl3 f a b c = ReaderT $ rot f a b c 
  where rot furn a b c gl  = furn gl a b c

gl4 f a b c d = ReaderT $ rot f a b c d
  where rot furn a b c d gl = furn gl a b c d

gl5 f a b c d e = ReaderT $ rot f a b c d e
  where rot furn a b c d e gl = furn gl a b c d e

gl6 h a b c d e f = ReaderT $ rot h a b c d e f
  where rot furn a b c d e f gl = furn gl a b c d e f

foreign import javascript unsafe "$1.clearColor($2, $3, $4, $5)" 
        js_glClearColor :: GL -> Float -> Float -> Float -> Float -> IO ()
clearColor = gl4 js_glClearColor

foreign import javascript unsafe "$1.enable($1.DEPTH_TEST)" js_glEnableDepthTest :: GL -> IO ()
enableDepthTest = gl0 js_glEnableDepthTest

foreign import javascript unsafe "$1.createShader($1.FRAGMENT_SHADER)" js_glCreateFragmentShader :: GL -> IO (Shader FragmentShader)
createVertexShader = gl0 js_glCreateVertexShader

foreign import javascript unsafe "$1.createShader($1.VERTEX_SHADER)" js_glCreateVertexShader :: GL -> IO (Shader VertexShader)
createFragmentShader = gl0 js_glCreateFragmentShader

foreign import javascript unsafe "$1.shaderSource($2,$3)" js_glShaderSource :: GL -> Shader a -> T.JSString -> IO (Shader a)
shaderSource :: (Shader a) -> T.JSString -> ReaderT GL IO (Shader a)
shaderSource = gl2 js_glShaderSource

foreign import javascript unsafe "$1.compileShader($2)" js_glCompileShader :: GL -> Shader a -> IO (Shader a)
compileShader = gl1 js_glCompileShader

foreign import javascript unsafe "$1.createProgram()" js_glCreateProgram :: GL -> IO ShaderProgram
createProgram = gl0 js_glCreateProgram

foreign import javascript unsafe "$1.attachShader($2,$3)" js_glAttachShader :: GL -> ShaderProgram -> (Shader a) -> IO ()
attachShader = gl2 js_glAttachShader

foreign import javascript unsafe "$1.linkProgram($2)" js_glLinkProgram :: GL -> ShaderProgram -> IO ()
linkProgram = gl1 js_glLinkProgram

foreign import javascript unsafe "$1.useProgram($2)" js_glUseProgram :: GL -> ShaderProgram -> IO ()
useProgram = gl1 js_glUseProgram

foreign import javascript unsafe "$1.enableVertexAttribArray($2)" js_glEnableVAA :: GL -> ShaderAttribute -> IO ()
enableVAA = gl1 js_glEnableVAA

foreign import javascript unsafe "$1.getAttribLocation($2,$3)" js_glGetAttribLocation :: GL -> ShaderProgram -> T.JSString -> IO ShaderAttribute
getAttribLocation = gl2 js_glGetAttribLocation

foreign import javascript unsafe "$1.getUniformLocation($2,$3)" js_glGetUniformLocation :: GL -> ShaderProgram -> T.JSString -> IO ShaderUniform
getUniformLocation = gl2 js_glGetUniformLocation

-- The second parameters to these change the result type...
foreign import javascript unsafe "$1.getShaderParameter($2,$1.COMPILE_STATUS)" js_glGetShaderCompileStatus :: GL -> (Shader a) -> IO T.JSBool
getShaderCompileStatus = gl1 js_glGetShaderCompileStatus

foreign import javascript unsafe "$1.getProgramParameter($2,$1.LINK_STATUS)" js_glGetProgramLinkStatus :: GL -> ShaderProgram -> IO T.JSBool
getProgramLinkStatus = gl1 js_glGetProgramLinkStatus

foreign import javascript unsafe "$1.getShaderInfoLog($2)" js_glGetShaderInfoLog :: GL -> Shader a -> IO T.JSString
getShaderInfoLog = gl1 js_glGetShaderInfoLog

foreign import javascript "mat4.create()" js_mat4Create :: IO Mat4
mat4 = gl0 (const js_mat4Create)

foreign import javascript "$1.uniformMatrix4fv($2, false, $3)" glUniformMatrix4fv :: GL -> ShaderUniform -> Mat4 -> IO ()
uniformMatrix4fv = gl2 glUniformMatrix4fv

foreign import javascript "$1.createBuffer()" js_glCreateBuffer :: GL -> IO Buffer
createBuffer = gl0 js_glCreateBuffer

foreign import javascript "$1.bindBuffer($1[$2], $3)" js_glBindBuffer :: GL -> BufferBinding -> Buffer -> IO ()
bindBuffer = gl2 js_glBindBuffer

foreign import javascript "'ARRAY_BUFFER'" arrayBuffer :: BufferBinding

-- For now, assume we're only buffering floats, with original... byte order? I guess?
-- and gl.STATIC_DRAW is a performance hint. We'll leave it for now.
-- BufferBinding done this way isn't quite a hack.
foreign import javascript "$1.bufferData($1[$2], new Float32Array($3), $1.STATIC_DRAW)" 
        js_glBufferData :: GL -> BufferBinding -> T.JSRef [Float] -> IO ()
bufferData = gl2 js_glBufferData

-- Note: We're using a very old version of glMatrix. Docs at https://github.com/toji/gl-matrix/blob/4cbb9339ee074a7ad7e65fa7b40ca279d5253eef/gl-matrix.js are closer than the current...
foreign import javascript "mat4.perspective($1, $2, $3, $4, $5)"
  js_mat4perspective :: Float -> Float -> Float -> Float -> Mat4 -> IO ()
mat4perspective = gl5 (const js_mat4perspective)

foreign import javascript "mat4.identity($1)" js_mat4identity :: Mat4 -> IO ()
mat4identity = gl1 (const js_mat4identity)

foreign import javascript "mat4.translate($1, $2)" js_mat4translate :: Mat4 -> T.JSRef [Float] -> IO ()
mat4translate = gl2 (const js_mat4translate)

-- gl.FLOAT should get sorted out with buffer types...
foreign import javascript "$1.vertexAttribPointer($2, $3, $1.FLOAT, false, 0, 0)"
  js_glVertexAttribPointer :: GL -> ShaderAttribute -> Int -> IO ()
vertexAttribPointer = gl2 js_glVertexAttribPointer

foreign import javascript "$1.drawArrays($1[$2], $3, $4)" js_glDrawArrays :: GL -> DrawType -> Int -> Int -> IO ()
drawArrays = gl3 js_glDrawArrays

foreign import javascript "'TRIANGLES'" drawTriangles :: DrawType
foreign import javascript "'TRIANGLE_STRIP'" drawTriangleStrip :: DrawType

foreign import javascript "$1.clear($1.COLOR_BUFFER_BIT | $1.DEPTH_BUFFER_BIT)" js_glClear :: GL -> IO ()
clear = gl0 js_glClear

foreign import javascript "$1.viewport($2, $3, $4, $5)" js_glViewport :: GL -> Int -> Int -> Int -> Int -> IO ()
viewport = gl4 js_glViewport

-- It's quite likely I'm doing this badly.
glList :: (MonadIO m) => [Float] -> m (T.JSRef [Float])
glList l = liftIO $ M.toJSRefListOf l

fragmentShaderSrc = 
  "    precision mediump float;                     \
  \    void main(void) {                            \
  \        gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0); \
  \    }                                            \
  \"

vertexShaderSrc = 
  "    attribute vec3 aVertexPosition;                                      \
  \    uniform mat4 uMVMatrix;                                              \
  \    uniform mat4 uPMatrix;                                               \
  \    void main(void) {                                                    \
  \        gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0); \
  \    }                                                                    \
  \"

data GLInfo = GLInfo { glContext :: GL
                     , glWidth :: Int
                     , glHeight :: Int
                     }

-- This should carry a type once we put other things in buffers...
data BufferInfo = BufferInfo { buffer :: Buffer
                             , numItems :: Int
                             , itemSize :: Int
                             }

data WhiteShader = WhiteShader { wsProgram :: ShaderProgram
                               , wsVertexP :: ShaderAttribute
                               , wsPMatrix :: ShaderUniform
                               , wsMVMatrix :: ShaderUniform
                               }

class ToBuffer f where
  toBuffer :: f -> GLIO BufferInfo

newtype V3L = V3L [(Float,Float,Float)]
instance ToBuffer V3L where
  toBuffer (V3L lst) = do
    b <- createBuffer
    bindBuffer arrayBuffer b
    bufferData arrayBuffer =<< glList ((\(a,b,c) -> [a,b,c]) =<< lst)
    return $ BufferInfo b (length lst) 3

initGL :: Canvas -> IO GLInfo
initGL canvas = GLInfo <$> js_getContext canvas <*> js_getCanvasWidth canvas <*> js_getCanvasHeight canvas

-- Help with string types.
glStr :: String -> T.JSString
glStr = F.toJSString

makeShader :: String -> GLIO (Shader a) -> GLIO (Shader a)
makeShader src create = do
  shader <- create
  shaderSource shader $ glStr src
  compileShader shader

  success <- F.fromJSBool <$> getShaderCompileStatus shader
  if success 
  then return shader
  else do
    log <- F.fromJSString <$> getShaderInfoLog shader
    error log
    
-- We could use an extra type for a compiled shader... Maybe later.
makeShaders :: GLIO (Shader VertexShader, Shader FragmentShader)
makeShaders = do
  fragmentShader <- makeShader fragmentShaderSrc createFragmentShader
  vertexShader <- makeShader vertexShaderSrc createVertexShader
  return (vertexShader, fragmentShader)

makeProgram :: (Shader VertexShader, Shader FragmentShader) -> GLIO WhiteShader
makeProgram (vertexShader, fragmentShader) = do
  program <- createProgram
  
  attachShader program vertexShader
  attachShader program fragmentShader
  linkProgram program

  linkSuccess <- F.fromJSBool <$> getProgramLinkStatus program

  when (not linkSuccess) $ error "Could not initialise shaders"

  useProgram program
  
  vertexPosition <- getAttribLocation program $ glStr "aVertexPosition"
  pMatrixU <- getUniformLocation program (glStr "uPMatrix")
  mvMatrixU <- getUniformLocation program (glStr "uMVMatrix")

  enableVAA vertexPosition
  
  return $ WhiteShader program vertexPosition pMatrixU mvMatrixU

initBuffers :: GLIO (BufferInfo, BufferInfo)
initBuffers = do
  triangle <- toBuffer $ V3L [ ( 0.0, 1.0,  0.0)
                             , (-1.0, -1.0, 0.0)
                             , (1.0,  -1.0, 0.0)
                             ]
  square <- toBuffer $ V3L [ ( 1.0,  1.0,  0.0)
                           , (-1.0,  1.0,  0.0)
                           , ( 1.0, -1.0,  0.0)
                           , (-1.0, -1.0,  0.0)
                           ]
  return (triangle, square)

drawScene :: (Int, Int) -> WhiteShader -> (BufferInfo, BufferInfo) -> GLIO ()
drawScene (width, height) shader (triangle, square) = do
  clear
  viewport 0 0 width height
  -- This will run without the call to viewport

  moveMatrix <- mat4
  positionMatrix <- mat4

  mat4perspective 45 (fromIntegral width / fromIntegral height) 0.1 100.0 positionMatrix

  mat4identity moveMatrix  

  -- move is a 3-element list...
  -- Some of the types here are still quite loose.
  let moveAndDraw move shape = do
        mat4translate moveMatrix =<< glList move
        bindBuffer arrayBuffer (buffer shape)
        vertexAttribPointer (wsVertexP shader) (itemSize shape) 
        uniformMatrix4fv (wsPMatrix shader) positionMatrix
        uniformMatrix4fv (wsMVMatrix shader) moveMatrix
        -- The original draws 'triangles' for the triangle and a strip for the square...
        -- Probably for didactic reasons. We can refactor that out when we need to.
        drawArrays drawTriangleStrip 0 (numItems shape)

  moveAndDraw [-1.4, 0.0, -7.0] triangle
  moveAndDraw [3.0, 0.0, 0.0] square

main = runLesson1 =<< initGL =<< js_documentGetElementById "lesson01-canvas"

runLesson1 glInfo = flip runReaderT (glContext glInfo) $ do
  clearColor 0.0 0.0 0.0 1.0
  enableDepthTest
  
  whiteShader <- makeProgram =<< makeShaders

  buffers <- initBuffers
  drawScene (glWidth glInfo, glHeight glInfo) whiteShader buffers
