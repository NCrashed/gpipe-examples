module Main where

import Graphics.GPipe
import Control.Monad (unless)
import Control.Arrow (first)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Exception (MonadException)

import qualified Graphics.GPipe.Context.GLFW as GLFW

main :: IO ()
main =
  runContextT GLFW.defaultHandleConfig $ do
    win <- newWindow (WindowFormatColor RGB8) (GLFW.defaultWindowConfig "Uniforms")
    vertexBuffer :: Buffer os (B4 Float, B3 Float) <- newBuffer 3
    writeBuffer vertexBuffer 0 [ (V4 (-1) 1 0 1, V3 1 0 0)
                               , (V4 0 (-1) 0 1, V3 0 1 0)
                               , (V4 1 1 0 1, V3 0 0 1)
                               ]

    uniformBuffer :: Buffer os (Uniform (B Float)) <- newBuffer 1

    shader <- compileShader $ do
      primitiveStream <- toPrimitiveStream id
      let primitiveStream2 = fmap (\(pos,clr) -> (pos - V4 1 1 0 0, clr / 10)) primitiveStream
      let primitiveStream3 = primitiveStream `mappend` primitiveStream2
      let rotationMatrix a = V4 (V4 (cos a) (-sin a) 0 0)
                                (V4 (sin a) (cos a) 0 0)
                                (V4 0    0    1 0)
                                (V4 0    0    0 1)
      uniform <- getUniform (const (uniformBuffer,0))
      let primitiveStream4 = fmap (first (rotationMatrix uniform !*)) primitiveStream3
      fragmentStream <- rasterize (const (FrontAndBack, ViewPort (V2 0 0) (V2 500 500), DepthRange 0 1)) primitiveStream4
      drawWindowColor (const (win, ContextColorOption NoBlending (V3 True True True))) fragmentStream

    loop vertexBuffer shader win uniformBuffer 0

loop :: (Color c Float ~ V3 a1, MonadIO m, MonadException m, ContextColorFormat c, Num a1) 
    => Buffer os a2 -- ^ Triangle buffer
    -> (PrimitiveArray Triangles a2 -> Render os ()) -- ^ Compiled shader
    -> Window os c ds  -- ^ Window
    -> Buffer os (Uniform (B Float)) -- ^ Uniform buffer for angle
    -> Float -- ^ Current angle value
    -> ContextT GLFW.Handle os m ()
loop vertexBuffer shader win uniformBuffer angle = do
  writeBuffer uniformBuffer 0 [angle]
  render $ do
    clearWindowColor win (V3 0 0 0)
    vertexArray <- newVertexArray vertexBuffer
    let primitiveArray = toPrimitiveArray TriangleList vertexArray
    shader primitiveArray
  swapWindowBuffers win

  closeRequested <- GLFW.windowShouldClose win
  unless (closeRequested == Just True) $
    loop vertexBuffer shader win uniformBuffer ((angle+0.1) `mod''` (2*pi))