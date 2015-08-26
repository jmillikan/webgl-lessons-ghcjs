module Simple where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Control.Applicative
import Control.Monad
import Control.Monad.Reader

import qualified GHCJS.Types    as T
import qualified GHCJS.Foreign  as F
import qualified GHCJS.Marshal  as M

import WebGL

-- Cleaner shader sources
src = QuasiQuoter { quoteExp = stringE }

{-
Lift our WebGL and glMatrix natives into a ReaderT with a GL context.

Throw in some utilities for dealing with GL types and buffers, and some 
functions we might reasonably use in every WebGL program.
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

clearColor = gl4 js_glClearColor
enableDepthTest = gl0 js_glEnableDepthTest
createVertexShader = gl0 js_glCreateVertexShader
createFragmentShader = gl0 js_glCreateFragmentShader
shaderSource :: (Shader a) -> T.JSString -> ReaderT GL IO (Shader a)
shaderSource = gl2 js_glShaderSource
compileShader = gl1 js_glCompileShader
createProgram = gl0 js_glCreateProgram
attachShader = gl2 js_glAttachShader
linkProgram = gl1 js_glLinkProgram
useProgram = gl1 js_glUseProgram
enableVAA = gl1 js_glEnableVAA
getAttribLocation = gl2 js_glGetAttribLocation
getUniformLocation = gl2 js_glGetUniformLocation
getShaderCompileStatus = gl1 js_glGetShaderCompileStatus
getProgramLinkStatus = gl1 js_glGetProgramLinkStatus
getShaderInfoLog = gl1 js_glGetShaderInfoLog
mat4 = gl0 (const js_mat4Create)
uniformMatrix4fv = gl2 glUniformMatrix4fv
createBuffer = gl0 js_glCreateBuffer
bindBuffer = gl2 js_glBindBuffer
bufferData = gl2 js_glBufferData
vertexAttribPointer = gl2 js_glVertexAttribPointer
drawArrays = gl3 js_glDrawArrays
clear = gl0 js_glClear
viewport = gl4 js_glViewport

mat4perspective = gl5 (const js_mat4perspective)
mat4identity = gl1 (const js_mat4identity)
mat4translate = gl2 (const js_mat4translate)
mat4set = gl2 (const js_mat4set)
mat4rotate = gl3 (const js_mat4rotate)

-- I'm lazy fairly certain GHCJS has better options for dealing with arrays and strings. Caveat emptor.

glList :: (MonadIO m) => [Float] -> m (T.JSRef [Float])
glList l = liftIO $ M.toJSRefListOf l

-- Help with string types.
glStr :: String -> T.JSString
glStr = F.toJSString

-- TBD: Where this belongs, whether it needs reformed...
data BufferInfo = BufferInfo { buffer :: Buffer
                             , numItems :: Int
                             , itemSize :: Int
                             }

class ToBuffer f where
  toBuffer :: f -> GLIO BufferInfo

-- Wrapping these as there's probably a better use for the raw types.

newtype V3L = V3L [(Float,Float,Float)]
instance ToBuffer V3L where
  toBuffer (V3L lst) = do
    b <- createBuffer
    bindBuffer arrayBuffer b
    bufferData arrayBuffer =<< glList ((\(a,b,c) -> [a,b,c]) =<< lst)
    return $ BufferInfo b (length lst) 3

newtype V4L = V4L [(Float,Float,Float,Float)]
-- Love-uh-ly flooooat, wonder flooooat
instance ToBuffer V4L where
  toBuffer (V4L lst) = do
    b <- createBuffer
    bindBuffer arrayBuffer b
    bufferData arrayBuffer =<< glList ((\(a,b,c,d) -> [a,b,c,d]) =<< lst)
    return $ BufferInfo b (length lst) 4

makeShader :: String -> GLIO (Shader a) -> GLIO (Shader a)
makeShader src create = do
  shader <- create
  shaderSource shader $ glStr src
  compileShader shader

  success <- F.fromJSBool <$> getShaderCompileStatus shader
  if success 
  then return shader
  else do
    error =<< F.fromJSString <$> getShaderInfoLog shader

buildProgram :: Shader VertexShader -> Shader FragmentShader -> GLIO ShaderProgram
buildProgram vertexShader fragmentShader = do
  program <- createProgram
  
  attachShader program vertexShader
  attachShader program fragmentShader
  linkProgram program

  linkSuccess <- F.fromJSBool <$> getProgramLinkStatus program

  when (not linkSuccess) $ error "Could not initialise shaders"

  return program

now = gl0 (const js_now)