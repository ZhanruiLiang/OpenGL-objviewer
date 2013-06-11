module State where

import Control.Monad
import Data.Maybe

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW

import Utils
import ObjRead
import Camera
import Menu

data State = State {
    stCamera :: Camera
  , stModel :: Maybe Model
  , stMenu :: Menu
  -- switches
  , stTextureEnable :: Bool
  , stLightEnable :: Bool
  , stMaterialEnable :: Bool
  , needQuit :: Bool
}

defaultState = State {
    stCamera = defaultCamera
  , stModel = Nothing
  , stMenu = defaultMenu
  , stTextureEnable = True
  , stLightEnable = True
  , stMaterialEnable = True
  , needQuit = False
}

onPress keyName state callback = do
  p <- GLFW.getKey keyName
  if p == GLFW.Press then callback state
  else return state

initState state = do
  texture Texture2D $= if1 (stTextureEnable state) Enabled Disabled
  lighting $= if1 (stLightEnable state) Enabled Disabled

update :: State -> IO State
update state = if isNothing (stModel state) then return state
  else let
    model = fromJust.stModel$ state
    cam = stCamera state
  in do
    -- btn <- GLFW.getMouseButton (GLFW.ButtonNo 3)
    wheel <- get GLFW.mouseWheel
    let state' = state { stCamera = zoom cam (1 + 0.05 * fromIntegral wheel) }
    state' <- onPress 'L' state' toggleLight
    state' <- onPress 'T' state' toggleTexture
    state' <- onPress 'M' state' toggleMaterial
    state' <- onPress 'Q' state' $ (\s-> return s{ needQuit = True })
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
  if enabled then 
    colorMaterial $= Just (Front, AmbientAndDiffuse)
  else
    colorMaterial $= Nothing
  return state { stMaterialEnable = not enabled }
