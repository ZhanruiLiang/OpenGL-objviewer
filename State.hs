module State where

import Control.Monad
import Data.Maybe

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW

import Utils
import ObjRead
import Camera
import Menu
import qualified Config as C

data State = State {
    stCamera :: Camera
  , stModel :: Maybe Model
  , stMenu :: Menu
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
  -- switches
  , stPersp = True
  , stTextureEnable = False
  , stLightEnable = True
  , stMaterialEnable = False
  , stWireEnable = False
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

update :: State -> IO State
update state = if isNothing (stModel state) then return state
  else let
    model = fromJust.stModel$ state
    cam = stCamera state
  in do
    wheel <- get GLFW.mouseWheel
    let cam' = zoom cam (1 + 0.05 * fromIntegral wheel)
        cam'' = if stAutoRotate state  then rotateCam cam' C.rotateSpeed else cam'
        state' = state { stCamera = cam'' }
    state' <- foldM (\s f-> f s) state' [
          onPress 'L' toggleLight
        , onPress 'T' toggleTexture
        , onPress 'M' toggleMaterial
        , onPress 'P' togglePersp
        , onPress 'W' toggleWire
        , onPress 'Q' (\s-> return s{ needQuit = True })
      ]
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
