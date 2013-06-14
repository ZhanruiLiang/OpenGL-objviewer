module Menu where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import Config as C
import Utils

data Menu = Menu {
    mColor :: Color4 GLfloat
}

defaultMenu = Menu {
    mColor = (Color4 0.2 0.2 0.2 0.6)
}

renderMenu menu state = do
    preservingAttrib [EnableAttributes, PolygonAttributes] $ do
      blend $= Enabled
      texture Texture2D $= Disabled
      lighting $= Disabled
      polygonMode $= (Fill, Fill)

      matrixMode $= Projection
      preservingMatrix $ do
        loadIdentity
        ortho2D 0 1 0 1

        matrixMode $= Modelview 0
        preservingMatrix $ do
          loadIdentity
          -- draw background
          renderPrimitive Quads $ do
            color (mColor menu)
            v 0 0 >> v 0 1 >> v C.menuWidth 1 >> v C.menuWidth 0
            let mc = mColor menu
                Color4 r g b _ = C.bgColor
                fadeC = c r g b 0
            color mc >> v C.menuWidth 0
            fadeC >> v (C.menuWidth+C.shadowWidth) 0
            fadeC >> v (C.menuWidth+C.shadowWidth) 1
            color mc >> v C.menuWidth 1
        matrixMode $= Projection
      matrixMode $= Modelview 0
  where
    v x y = vertex (Vertex3 x (y::GLfloat) 0)
    c r g b a = color (Color4 r g b (a::GLfloat))
    t x y = texCoord (TexCoord2 x (y::GLfloat))
