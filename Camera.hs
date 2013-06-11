module Camera (
    Camera(..)
  , modifyStack
  , adjustCam
  , defaultCamera
  , zoom
  )where
import Graphics.Rendering.OpenGL
import ObjRead
import Config as C

data Camera = Camera {
    eyePos :: Vertex3 GLdouble
  , centerPos :: Vertex3 GLdouble
  , camScale :: GLfloat
}

defaultCamera = Camera {
    eyePos = Vertex3 1.0 1.0 1.0
  , centerPos = Vertex3 0.0 0.0 0.0
  , camScale = 1.0
}

-- instance Num a => Num (Vector3 a) where
--   (Vector3 x y z) + (Vector3 x' y' z') = Vector3 (x+x') (y+y') (z+z')
--   (Vector3 x y z) - (Vector3 x' y' z') = Vector3 (x-x') (y-y') (z-z')

negate' (Vector3 x y z) = Vector3 (-x) (-y) (-z)

originVec = Vector3 0.0 0.0 0.0

verToVec (Vertex3 x y z) = Vector3 x y z

modifyStack :: Camera -> IO ()
modifyStack cam = do
  let r = camScale cam
      cp = centerPos cam
      ep = eyePos cam
      up = Vector3 0.0 0.0 1.0
  lookAt ep cp up
  scale r r r
  -- translate $ negate' (verToVec cp)

lookAt' e c u = lookAt (conv e) (conv c) (conv' u) where
  conv (Vertex3 x y z) = Vertex3 (rf x) (rf y) (rf z)
  conv' (Vector3 x y z) = Vector3 (rf x) (rf y) (rf z)

rf = fromRational . toRational

adjustCam :: Camera -> Model -> Camera
adjustCam cam model = cam {
      eyePos = eyePos'
    , centerPos = centerPos'
  }
  where
    ((Vertex3 cx cy cz), (Vector3 sx sy sz)) = boundingBox model
    eyePos' = Vertex3 (rf$ cx+sx*2) (rf$ cy+sy*2) (rf$ cz+sz*2)
    centerPos' = Vertex3 (rf$ cx) (rf$ cy) (rf$ cz)

boundingBox model = (
    -- center
    Vertex3 ((x1+x2)/2) ((y1+y2)/2) ((z1+z2)/2),
    -- lengths
    Vector3 ((x2-x1)/2) ((y2-y1)/2) ((z2-z1)/2))
  where
    mL f xs = foldl f (head xs) xs
    getX (Vertex3 x _ _) = x
    getY (Vertex3 _ y _) = y
    getZ (Vertex3 _ _ z) = z
    vlist obj = getVs.objBuffers$ obj
    getVs (v, _, _) = v
    x1 = mL min.map getX.concat.map vlist$ model
    y1 = mL min.map getY.concat.map vlist$ model
    z1 = mL min.map getZ.concat.map vlist$ model
    x2 = mL max.map getX.concat.map vlist$ model
    y2 = mL max.map getY.concat.map vlist$ model
    z2 = mL max.map getZ.concat.map vlist$ model

zoom cam scale = cam { camScale = scale' }
  where scale' = max C.minScale  (min C.maxScale scale)
