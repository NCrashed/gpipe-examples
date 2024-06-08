module Main where 

import Control.Applicative (pure)
import Control.Monad (unless)
import Control.Monad.Exception (MonadException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word (Word32)
import Graphics.GPipe

import qualified Graphics.GPipe.Context.GLFW as GLFW

main :: IO ()
main = runContextT GLFW.defaultHandleConfig $ do
  win <- newWindow (WindowFormatColor RGB8) (GLFW.defaultWindowConfig "Checkers")
  winSize <- maybe 0 (uncurry V2) <$> GLFW.getWindowSize win  
  vertexBuffer :: Buffer os (B2 Float) <- newBuffer 4
  writeBuffer vertexBuffer 0 [V2 0 0, V2 1 0, V2 0 1, V2 1 1]
  tex <- newTexture2D R8 (V2 15 15) 1
  let whiteBlack = cycle [minBound,maxBound] :: [Word32]
      blackWhite = drop 1 whiteBlack
  writeTexture2D tex 0 0 (V2 15 15) (cycle (take 15 whiteBlack ++ take 15 blackWhite))
  shader <- compileShader $ do
    primitiveStream <- toPrimitiveStream primitives 
    let primitiveStream2 = fmap (\pos2d -> (make3d pos2d, pos2d)) primitiveStream
    fragmentStream <- rasterize (\s -> (FrontAndBack, ViewPort (V2 0 0) (curPost s), DepthRange 0 1)) primitiveStream2
    let filter = SamplerFilter Nearest Nearest Nearest Nothing
        edge = (pure Repeat, undefined)
    samp <- newSampler2D (const (tex, filter, edge))
    let sampleTexture = pure . sample2D samp SampleAuto Nothing Nothing
        fragmentStream2 = fmap sampleTexture fragmentStream
    drawWindowColor (const (win, ContextColorOption NoBlending (pure True))) fragmentStream2

  loop vertexBuffer shader win 

make3d :: Floating a => V2 a -> V4 a
make3d (V2 x y) = projMat !*! viewMat !* V4 x y 0 1
  where
    viewMat = lookAt' (V3 1 2 2) (V3 0.5 0.5 0) (V3 0 1 0)
    projMat = perspective (pi/3) 1 1 100

-- Copy of lookAt from linear with normalize replaced with signorm
lookAt' :: Floating a => V3 a -> V3 a -> V3 a -> V4 (V4 a)
lookAt' eye center up =
  V4 (V4 xa.x xa.y xa.z xd)
     (V4 ya.x ya.y ya.z yd)
     (V4 (-za.x) (-za.y) (-za.z) zd)
     (V4 0     0     0     1)
  where za = signorm $ center - eye
        xa = signorm $ cross za up
        ya = cross xa za
        xd = -dot xa eye
        yd = -dot ya eye
        zd = dot za eye


loop :: (Color c Float ~ V3 a, MonadIO m, MonadException m, ContextColorFormat c, Num a) => Buffer os b -> (ShaderEnv b -> Render os ()) -> Window os c ds -> ContextT GLFW.Handle os m ()
loop vertexBuffer shader win = do
  cursorPos <- maybe 0 (uncurry V2) <$> GLFW.getCursorPos win
  let curPos = fmap round cursorPos
  render $ do
    clearWindowColor win (V3 0 0 0)
    vertexArray <- newVertexArray vertexBuffer
    let primitiveArray = toPrimitiveArray TriangleStrip vertexArray
    shader $ ShaderEnv primitiveArray curPos
  swapWindowBuffers win
  closeRequested <- GLFW.windowShouldClose win
  unless (closeRequested == Just True) $
    loop vertexBuffer shader win  

data ShaderEnv b = ShaderEnv {
    primitives :: PrimitiveArray Triangles b
  , curPost :: V2 Int
  }
