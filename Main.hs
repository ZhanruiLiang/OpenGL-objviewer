module Main where

import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL as GL
import Control.Monad
import Data.Maybe
import Data.IORef
import System.IO
import qualified Data.Sequence as S

import Utils
import ObjRead
import Camera
import State
import Menu
import Config

defaultObjColor = Color4 0.2 0.2 0.3 (1.0 :: GLfloat)

renderObj :: State -> Object -> IO ()
renderObj state obj = do
  let hasMat = isJust.objMaterial$ obj
  let material = fromJust.objMaterial$ obj
  preservingMatrix $ do
    when hasMat $ do
      when (stMaterialEnable state) $ do
        materialAmbient Front $= mtlAmbient material
        materialDiffuse Front $= mtlDiffuse material
        materialSpecular Front $= mtlSpecular material
        materialShininess Front $= mtlShiness material

      when (stTextureEnable state) $ do
        textureBinding Texture2D $= mtlImage material
      
    renderPrimitive Triangles $ do
      color defaultObjColor
      let (vs, ts, ns) = objBuffers obj
      if (not.null$ ts) && hasMat then
        forM_ (zip3 vs ts ns) (\(v, t, n) -> do
          texCoord t
          normal n
          vertex v
          )
      else 
        forM_ (zip vs ns) (\(v, n) -> do
          normal n
          vertex v
          )
      return ()

display state = do
  let cam = stCamera state
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  modifyStack cam
  preservingMatrix $ do
    case stModel state of
      Nothing -> return()
      Just model -> do
        forM_ model (renderObj state)
  renderMenu (stMenu state) state
  GLFW.swapBuffers

reshape size@(Size w h) = do
  viewport $= (Position 0 0, size)
  matrixMode $= Projection
  loadIdentity
  perspective 60 (fromIntegral w / fromIntegral h) 0.1 200.0
  matrixMode $= Modelview 0
  loadIdentity

glInit = do
  GLFW.initialize
  GLFW.openWindow (Size 300 300) [
    GLFW.DisplayRGBBits 8 8 8,
    GLFW.DisplayAlphaBits 8,
    GLFW.DisplayDepthBits 24] GLFW.Window

  GLFW.swapInterval $= 1 -- vsync
  GLFW.windowTitle $= ""
  GLFW.windowSizeCallback $= reshape

  clearColor $= bgColor
  shadeModel $= Smooth
  depthFunc $= Just Less
  lighting $= Enabled
  light (Light 0) $= Enabled
  let r = 20.0 :: GLfloat
  position (Light 0) $= Vertex4 r r r 1.0
  normalize $= Enabled

loadModel' :: State -> FilePath -> IO State
loadModel' state path = do
  model <- loadModel path
  return state { 
      stModel = Just model
    , stCamera = adjustCam (stCamera state) model
    }

main = do
  glInit
  initState defaultState
  initMenu defaultState
  mainLoop =<< loadModel' defaultState defaultModelPath
  GLFW.closeWindow
  GLFW.terminate

defaultModelPath = "models/emitter.obj"

mainLoop state = do
  GLFW.waitEvents
  state' <- update state
  when (not.needQuit$ state) $ do
    display state'
    mainLoop state'
