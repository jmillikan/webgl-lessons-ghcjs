{-# LANGUAGE JavaScriptFFI      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes  #-}
{-# LANGUAGE RecursiveDo #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Data.IORef

import qualified GHCJS.Types    as T
import qualified GHCJS.Foreign  as F
import qualified GHCJS.Marshal  as M

import WebGL
import Simple

fragmentShaderSrc = [src|
  precision mediump float;

  varying vec4 vColor;

  void main(void) {
    gl_FragColor = vColor;
  }
|]

vertexShaderSrc = [src|
  attribute vec3 aVertexPosition;
  attribute vec4 aVertexColor;

  uniform mat4 uMVMatrix;
  uniform mat4 uPMatrix;

  varying vec4 vColor;

  void main(void) {
    gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);
    vColor = aVertexColor;
  }
|]

data GLInfo = GLInfo { glContext :: GL
                     , glWidth :: Int
                     , glHeight :: Int
                     }

data ColorShader = ColorShader { wsProgram :: ShaderProgram
                               , wsVertexP :: ShaderAttribute
                               , sVertexColor :: ShaderAttribute
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

makeProgram :: (Shader VertexShader, Shader FragmentShader) -> GLIO ColorShader
makeProgram (vertexShader, fragmentShader) = do
  program <- buildProgram vertexShader fragmentShader

  useProgram program

  vertexPosition <- getAttribLocation program $ glStr "aVertexPosition"
  vertexColor <- getAttribLocation program $ glStr "aVertexColor"
  pMatrixU <- getUniformLocation program (glStr "uPMatrix")
  mvMatrixU <- getUniformLocation program (glStr "uMVMatrix")

  enableVAA vertexPosition
  enableVAA vertexColor

  return $ ColorShader program vertexPosition vertexColor pMatrixU mvMatrixU

initBuffers :: GLIO (BufferInfo, BufferInfo, BufferInfo, BufferInfo)
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
  triangleColors <- toBuffer $ V4L [ (1.0, 0.0, 0.0, 1.0)
                                   , (0.0, 1.0, 0.0, 1.0)
                                   , (0.0, 0.0, 1.0, 1.0)
                                   ]
  squareColors <- toBuffer $ V4L $ replicate 4 (0.5, 0.5, 1.0, 1.0)
  return (triangle, square, triangleColors, squareColors)

degToRad degrees = degrees * 3.14159 / 180

-- This is becoming excessive...
drawScene :: (Int, Int) -> ColorShader -> (BufferInfo, BufferInfo, BufferInfo, BufferInfo) -> SceneState -> GLIO ()
drawScene (width, height) shader (triangle, square, triangleColors, squareColors) (SceneState _ rTri rSquare) = do
  clear
  viewport 0 0 width height
  -- This will run without the call to viewport

  moveMatrix <- mat4
  positionMatrix <- mat4

  mat4perspective 45 (fromIntegral width / fromIntegral height) 0.1 100.0 positionMatrix

  mat4identity moveMatrix

  -- I've chosen to structure the drawing  a bit more than the original. 
  -- Approximately the same calls to both WebGL and glMatrix are made.

  let draw moveMatrix shape colors (rotDegrees, axis) = do
        -- In the original, moveMatrix is a global that points to the top of a stack.
        -- This in the call-stack is more functional, though we're doing it with a lot of IO.
        -- We're passing in moveMatrix explicitly since that's a bit closer to what he's doing.

        transformMatrix <- mat4
        mat4set moveMatrix transformMatrix
        mat4rotate transformMatrix (degToRad rotDegrees) =<< glList axis

        bindBuffer arrayBuffer (buffer shape)
        vertexAttribPointer (wsVertexP shader) (itemSize shape)
        bindBuffer arrayBuffer (buffer colors)
        vertexAttribPointer (sVertexColor shader) (itemSize colors)
        uniformMatrix4fv (wsPMatrix shader) positionMatrix
        uniformMatrix4fv (wsMVMatrix shader) transformMatrix
        -- The original draws 'triangles' for the triangle and a strip for the square...
        -- Probably for educational purposes.
        drawArrays drawTriangleStrip 0 (numItems shape)

  -- The types here are awful. I think GHCJS will do tuple -> list, should look into that...  
  let move mv = do
        mat4translate moveMatrix =<< glList mv

  move [-1.5, 0.0, -7.0] 
  draw moveMatrix triangle triangleColors (rTri, [0, 1, 0])
  move [3.0, 0.0, 0.0] 
  draw moveMatrix square squareColors (rSquare, [1, 0, 0])

main = runLesson1 =<< initGL =<< js_documentGetElementById "lesson01-canvas"

-- The original uses mutable state, so we will too.
data SceneState = SceneState { lastTime :: Double
                             , rTri :: Double
                             , rSquare :: Double
                             }

updateState newTime old = SceneState { lastTime = newTime
                                     , rTri = rTri old + (90 * elapsed) / 1000.0
                                     , rSquare = rSquare old + (75 * elapsed) / 1000.0
                                     }
  where elapsed = newTime - lastTime old

runLesson1 glInfo = flip runReaderT (glContext glInfo) $ do
  clearColor 0.0 0.0 0.0 1.0
  enableDepthTest

  shaderProgram <- makeProgram =<< makeShaders

  buffers <- initBuffers

  tInit <- now 

  stateRef <- liftIO $ newIORef $ SceneState tInit 0 0

  let tick = do
        tNow <- now
        liftIO $ modifyIORef stateRef $ updateState tNow
        
        st <- liftIO $ readIORef stateRef
        drawScene (glWidth glInfo, glHeight glInfo) shaderProgram buffers st

  tick

  -- The original directly calls requestAnimframe each tick.
  -- the bit from luite in WebGL.animate calls it forever.

  gl <- ask
  liftIO $ mdo
    callback <- F.syncCallback F.AlwaysRetain False $ do
      F.release callback -- h$releaseAll missing - this doesn't seem to leak though.
      flip runReaderT gl tick
    animate callback
  
