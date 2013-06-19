module State where

import Control.Monad
import Data.Maybe
import Data.IORef

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW

import Utils
import ObjRead
import Camera
import Menu
import qualified Config as C
import HalfEdge
import LoopSubdiv

data State = State {
    stCamera :: Camera
  , stModel :: Maybe Model
  , stMenu :: Menu
  , stSubDivTime :: Int
  -- switches
  , stPersp :: Bool
  , stTextureEnable :: Bool
  , stLightEnable :: Bool
  , stMaterialEnable :: Bool
  , stWireEnable :: Bool
  , stAutoRotate :: Bool
  , needQuit :: Bool
}

defaultState = State {
    stCamera = defaultCamera
  , stModel = Nothing
  , stMenu = defaultMenu
  , stSubDivTime = 0
  -- switches
  , stPersp = True
  , stTextureEnable = False
  , stLightEnable = True
  , stMaterialEnable = False
  , stWireEnable = True
  , stAutoRotate = True
  , needQuit = False
}

onPress keyName callback state = do
  p <- GLFW.getKey keyName
  if p == GLFW.Press then callback state
  else return state

-- | Init by toggle twice
initState state = do
  mapM_ (\f -> f =<< f state) [
      toggleTexture
    , toggleLight
    , toggleWire
    , toggleMaterial
    , togglePersp
    ]

subdiv :: State -> IO State
subdiv s = do
  debug$ "======================subdiv============="
  let model = stModel s
      divTime = stSubDivTime s
  debug$ "subdiv count: " ++ show divTime
  vcount <- newIORef (0::Int)
  fcount <- newIORef (0::Int)
  model' <- case model of
    Nothing -> return model
    Just m  -> return =<< (liftM Just) $ forM m $ \obj -> do
      let hs = objHS obj
          hs' = loopSubdiv hs
      obj' <- convertObj $ obj { objHS = hs' }
      modifyIORef fcount (+(length.hsFaces$hs)) 
      modifyIORef vcount (+(length.allVertices$hs)) 
      return$! obj'
  vc <- readIORef vcount
  fc <- readIORef fcount
  debug$ "fc: "++show fc++" vc:"++show vc
  return s { stModel = model', stSubDivTime = (divTime + 1) }

disable a b s = if stSubDivTime s > 0 && a s then return =<< b s else return s
enable a b s = if stSubDivTime s > 0 && (not.a$ s) then return =<< b s else return s

update :: State -> IO State
update state = if isNothing (stModel state) then return state
  else let
    model = fromJust.stModel$ state
    cam = stCamera state
  in do
    wheel <- get GLFW.mouseWheel
    (Position mouseX mouseY) <- get GLFW.mousePos
    (Size width height) <- get GLFW.windowSize
    let cam' = zoom cam (1 + 0.05 * fromIntegral wheel)
        -- cam'' = if stAutoRotate state  then rotateCam cam' C.rotateSpeed else cam'
        cam'' = rotateCam cam'  
            (360 * (fromIntegral mouseX) / (fromIntegral width)) 
            -- (360 * (0.5 + (fromIntegral mouseY) / (fromIntegral height))) 
            0

        state' = state { stCamera = cam'' }
    state' <- foldM (\s f-> f s) state' [
          onPress 'L' toggleLight
        , onPress 'T' toggleTexture
        , onPress 'M' toggleMaterial
        , onPress 'P' togglePersp
        , onPress 'W' toggleWire
        , onPress 'Q' (\s-> return s{ needQuit = True })
        , onPress 'S' subdiv
      ]
    state' <- enable stWireEnable toggleWire 
           =<< disable stTextureEnable toggleTexture 
           =<< disable stLightEnable toggleLight
           =<< disable stMaterialEnable toggleMaterial
           state'
    return state'

toggleTexture, toggleLight, toggleMaterial :: State -> IO State

if1 p a b = if p then a else b

toggleTexture state = let enabled = stTextureEnable state in do
  texture Texture2D $= if1 enabled Disabled Enabled
  when (not enabled) $ do
    textureFunction $= Modulate
  return state { stTextureEnable = not enabled }

toggleLight state = let enabled = stLightEnable state in do
  lighting $= if1 enabled Disabled Enabled
  return state { stLightEnable = not enabled }

toggleMaterial state = let enabled = stMaterialEnable state in do
  if enabled then do
    materialAmbient Front $= C.defaultObjColor
    materialDiffuse Front $= C.defaultObjColor
    materialSpecular Front $= Color4 0.1 0.1 0.1 1.0
    materialShininess Front $= 90
  else return()
  return state { stMaterialEnable = not enabled }

togglePersp state = let enabled = stPersp state in do
  matrixMode $= Projection
  loadIdentity
  (_, Size w h) <- get viewport
  -- rate = h / w => h = w * rate
  let rate = (fromIntegral h / fromIntegral w)
  if enabled then do
    ortho (-0.4) (0.4) (-0.4 * rate) (0.4 * rate) (0.1) (200.0)
    scale 0.2 0.2 (0.2 :: GLfloat)
  else do
    -- perspective 60 (fromIntegral w / fromIntegral h) 0.1 200.0
    perspective 60.0 (1.0/rate) 0.1 200.0
  matrixMode $= Modelview 0
  return state { stPersp = not enabled }

toggleWire state = let enabled = stWireEnable state in do
  polygonMode $= if1 enabled (Fill, Fill) (Line, Line)
  return state { stWireEnable = not enabled }
