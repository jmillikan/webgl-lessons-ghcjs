{-# LANGUAGE JavaScriptFFI      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Reader

import qualified GHCJS.Types    as T
import qualified GHCJS.Foreign  as F
import qualified GHCJS.Marshal  as M

-- Use our own WebGL/glMatrix bindings through a little utiltity/reader library.
import WebGL
import Simple

-- We could type these as vertex/fragment shader sources with a little template haskell work...
-- That would be cool, but not incredibly useful.
fragmentShaderSrc = [src|
    precision mediump float;
    void main(void) {
        gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);
    }
|]

vertexShaderSrc = [src|
    attribute vec3 aVertexPosition;
    uniform mat4 uMVMatrix;
    uniform mat4 uPMatrix;
    void main(void) {
        gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);
    }
|]

data GLInfo = GLInfo { glContext :: GL
                     , glWidth :: Int
                     , glHeight :: Int
                     }

data WhiteShader = WhiteShader { wsProgram :: ShaderProgram
                               , wsVertexP :: ShaderAttribute
                               , wsPMatrix :: ShaderUniform
                               , wsMVMatrix :: ShaderUniform
                               }

initGL :: Canvas -> IO GLInfo
initGL canvas = GLInfo <$> js_getContext canvas <*> js_getCanvasWidth canvas <*> js_getCanvasHeight canvas

-- We could use an extra type for a compiled shader... Maybe later.
makeShaders :: GLIO (Shader VertexShader, Shader FragmentShader)
makeShaders = do
  fragmentShader <- makeShader fragmentShaderSrc createFragmentShader
  vertexShader <- makeShader vertexShaderSrc createVertexShader
  return (vertexShader, fragmentShader)

makeProgram :: (Shader VertexShader, Shader FragmentShader) -> GLIO WhiteShader
makeProgram (vertexShader, fragmentShader) = do
  program <- buildProgram vertexShader fragmentShader

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
        -- The original draws 'triangles' for the triangle and a triangle strip for the square...
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
