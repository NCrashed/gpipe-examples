module Main where

import Control.Monad (unless)
import Control.Monad.Exception (MonadException)
import Control.Monad.IO.Class ( MonadIO ) 
import Graphics.GPipe
import Graphics.GPipe.Context.GLFW as GLFW

main :: IO ()
main =
  runContextT GLFW.defaultHandleConfig $ do
    win <- newWindow (WindowFormatColor RGB8) (GLFW.defaultWindowConfig "Hello world!")

    vertexBuffer :: Buffer os (B4 Float, B3 Float) <- newBuffer 3
    writeBuffer vertexBuffer 0 [ (V4 (-1) 1 0 1, V3 1 0 0)
                               , (V4 0 (-1) 0 1, V3 0 1 0)
                               , (V4 1 1 0 1,  V3 0 0 1)
                               ]

    shader <- compileShader $ do
      primitiveStream <- toPrimitiveStream primitives
      fragmentStream <- rasterize (\s -> (FrontAndBack, ViewPort (V2 0 0) (winSize s), DepthRange 0 1)) primitiveStream
      drawWindowColor (const (win, ContextColorOption NoBlending (V3 True True True))) fragmentStream

    loop vertexBuffer shader win

loop :: (Color c Float ~ V3 a, MonadIO m, MonadException m, ContextColorFormat c, Num a) => Buffer os b -> (ShaderEnv b -> Render os ()) -> Window os c ds -> ContextT Handle os m ()
loop vertexBuffer shader win = do
  winSize <- maybe 0 (uncurry V2) <$> GLFW.getWindowSize win 
  render $ do
    clearWindowColor win (V3 0 0 0)
    vertexArray <- newVertexArray vertexBuffer
    let primitiveArray = toPrimitiveArray TriangleList vertexArray
    shader $ ShaderEnv primitiveArray winSize 
  swapWindowBuffers win

  closeRequested <- GLFW.windowShouldClose win
  unless (closeRequested == Just True) $
    loop vertexBuffer shader win   

data ShaderEnv b = ShaderEnv {
    primitives :: PrimitiveArray Triangles b
  , winSize :: V2 Int
  }