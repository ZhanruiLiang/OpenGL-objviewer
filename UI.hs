import Graphics.Rendering.OpenGL (Vertex2, Color4, Size)

type Color = Color4 GLfloat

class Renderable a where
  render :: a -> IO ()
  fgcolor, bgcolor :: a -> Color
  name :: a -> String
  text :: a -> String
  boxSize :: a -> Size Int

data Widget = Button
            | CheckBox
            | TextInput
            | TextLabel
