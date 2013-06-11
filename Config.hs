module Config where
import Graphics.Rendering.OpenGL as GL

bgColor = GL.Color4 0.7 0.7 0.68 (1.0 :: GL.GLfloat)
shadowWidth = menuWidth * 0.15 :: GLfloat
menuWidth = 0.06 :: GLfloat
maxScale = 5.0 :: GLfloat
minScale = 0.2 :: GLfloat
