module Menu where
import Graphics.Rendering.OpenGL
import Config as C

data Menu = Menu {
    mColor :: Color4 GLfloat
}

defaultMenu = Menu {
    mColor = (Color4 0.2 0.2 0.2 0.6)
}

initMenu menu = do
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  blend $= Disabled

renderMenu menu state = do
    preservingAttrib [EnableAttributes] $ do
      blend $= Enabled
      lighting $= Disabled
      texture Texture2D $= Disabled
      matrixMode $= Projection
      preservingMatrix $ do
        loadIdentity
        ortho2D 0 1 0 1

        matrixMode $= Modelview 0
        preservingMatrix $ do
          loadIdentity
          color (mColor menu)
          renderPrimitive Quads $ do
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
    v x y = vertex (Vertex2 x (y::GLfloat))
    c r g b a = color (Color4 r g b (a::GLfloat))
