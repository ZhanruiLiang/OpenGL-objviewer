module ObjRead (
    loadModel
  , Object(..)
  , Model(..)
  , Material(..)
  , convertObj
  )where

import Data.Maybe
import Control.Monad
import System.FilePath
import Foreign (Ptr, newArray)
import qualified Data.Array.IArray as IA
import qualified Data.Map.Lazy as M
import qualified Data.Sequence as S
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW

import BufTypes
import HalfEdge
import Utils
import System.TimeIt

-- | Material data type
data Material = Material {
     mtlName :: String
  , mtlAmbient :: Color4 GLfloat
  , mtlDiffuse :: Color4 GLfloat
  , mtlSpecular :: Color4 GLfloat
  , mtlShiness :: GLfloat
  , mtlImage :: Maybe Image
} deriving (Show)

defaultMtl = Material {
    mtlName = ""
  , mtlAmbient = Color4 0.2 0.2 0.2 1.0
  , mtlDiffuse = Color4 0.2 0.2 0.2 1.0
  , mtlSpecular = Color4 0.2 0.2 0.2 1.0
  , mtlShiness = 10.0
  , mtlImage = Nothing
}

-- | Image data type
type Image = TextureObject
loadImage :: FilePath -> IO Image
loadImage path = do
  [texName] <- genObjectNames 1
  textureBinding Texture2D $= Just texName
  loaded <- GLFW.loadTexture2D path [BuildMipMaps]
  if loaded then putStrLn $ "loaded " ++ path
  else putStrLn $ "failed to load " ++ path
  textureFilter Texture2D $= ((Linear', Just Linear'), Linear')
  return texName
        
-- | Material Lib data type
data Mtllib = Mtllib {
  libMaterials :: M.Map String Material
} deriving (Show) 

emptyMtllib = Mtllib M.empty
lookupMaterial lib key = M.lookup key (libMaterials lib)
addMtl lib mtl = lib { libMaterials = M.insert (mtlName mtl) mtl (libMaterials lib) }

loadMtllib :: FilePath -> IO Mtllib
loadMtllib path = do
    datas <- readFile path
    (mtl, lib) <- foldM parseLine (defaultMtl, emptyMtllib) (filter (not.null.words) $ lines datas)
    return (addMtl lib mtl)
  where
    parseLine (mtl, lib) line = let
      toks = words line
      (cmd, args) = (head toks, tail toks)
      [r,g,b] = map read $ take 3 args 
      name = args !! 0
      in case cmd of
        "newmtl" -> return (defaultMtl { mtlName = name }, addMtl lib mtl)
        "Ka" -> return (mtl { mtlAmbient = Color4 r g b 1.0 } , lib)
        "Kd" -> return (mtl { mtlDiffuse = Color4 r g b 1.0 }, lib) 
        "Ks" -> return (mtl { mtlSpecular = Color4 r g b 1.0 }, lib) 
        "Ns" -> return (mtl { mtlShiness = min 128.0 (read (args!!0)) }, lib)
        "map_Kd" -> do
          let dir = fst.splitFileName $ path
          let imagePath = joinPath [dir, name]
          image <- loadImage imagePath
          return (mtl { mtlImage = Just image }, lib)
        _ -> return (mtl, lib)

-- | Object data type
data Object = Object {
    objName :: String
  -- , objBuffers :: CBuffers
  , objHS :: HStructure
  , objRender :: IO ()
  , objMaterial :: Maybe Material
}

-- | Model data type
type Model = [Object]

addObject :: Object -> Model -> Model
addObject obj = (obj:)

getObject :: String -> Model -> Object
getObject name = head.filter ((name==).objName)

emptyModel = []

data ModelState = ModelState {
    mstVBuffers :: VBuffers
  , mstIFaces :: [IFace]
  , mstMtllib :: Maybe Mtllib
  , mstMaterial :: Maybe Material
  , mstObjName :: String
  , mstModel :: Model
}

emptyMst = ModelState {
    mstVBuffers = emptyVbufs
  , mstIFaces = []
  , mstMtllib = Nothing
  , mstMaterial = Nothing
  , mstObjName = ""
  , mstModel = emptyModel
}

convertObj :: Object -> IO Object
convertObj obj = do
  let hs = objHS obj
  -- debug $ "makeInterleave"
  (n, ita) <- makeInterleave (hsFaces hs)
  indices <- newArray $ if n > 0 then [0..fromIntegral (n-1)::GLuint] else []
  return obj { objRender = render ita n indices }

addObject' allowNull s = if allowNull || (not.null.mstObjName$ s) then do
      let hs = fromIndexSet (mstVBuffers s) (mstIFaces s)
      obj <- convertObj $ Object {
              objRender = return ()
            , objName = mstObjName s
            , objHS = hs
            , objMaterial = mstMaterial s }
      let model' = addObject obj (mstModel s)
      return s { 
            mstIFaces = []
          , mstModel = model'
        }
  else return s {mstIFaces = []}

render :: Ptr GLfloat -> GLsizei -> Ptr GLuint -> IO ()
render a n idx = do
  interleavedArrays T2fN3fV3f 0 a
  drawElements Triangles n UnsignedInt idx

loadModel :: FilePath -> IO Model
loadModel path = do
    datas <- readFile path
    s <- foldM parseLine emptyMst (lines datas)
    mstModel `liftM` (addObject' True s)
  where 
    parseLine :: ModelState -> String -> IO ModelState
    parseLine s line = let
      (cmd, args) = (head toks, tail toks)
      toks = words line
      mtllib = fromJust$ mstMtllib s
      flushObj s args = do
          s' <- addObject' False s
          return s' { mstObjName = head args }
      in return =<< case cmd of
        -- o objName
        "o" -> flushObj s args
        "g" -> flushObj s args
        -- usemtl mtlName
        "usemtl" -> return s { mstMaterial = lookupMaterial mtllib (args!!0) }
        -- v x y z
        "v" -> let [x, y, z] = map read $ take 3 args
          in return$ appendVbuf (Just$Vertex3 x y z, Nothing, Nothing) s
        -- vn nx ny nz
        "vn" -> let [x, y, z] = map read $take 3 args
          in return$ appendVbuf (Nothing, Nothing, Just$Normal3 x y z) s 
        -- vt tx ty
        "vt" -> let [x, y] = map read $ take 2 args
          in return$ appendVbuf (Nothing, Just$TexCoord2 x y, Nothing) s 
        -- mtllib libName
        "mtllib" -> do
          let dir = fst.splitFileName $ path
          mtllib <- loadMtllib (joinPath [dir, args!!0])
          return$ s { mstMtllib = Just mtllib }
        -- f i1 i2 i3 # triangle faces
        "f" | length args == 3 -> do
                return$ appendFace (map conv. map (splitBy '/') $ args) s
            | otherwise -> error "Not a triangular face"
              where 
                conv [v, t, n] = (fromJust.sconv $v, sconv t, sconv n)
                conv g = error $ "invalid index tuple length:" ++ show g
                sconv "" = Nothing 
                sconv s = Just $ (read s - 1 :: Int)
        _ -> return s
    appendVbuf x s = s { mstVBuffers = bufferAppend x (mstVBuffers s) }
    appendFace f s = s { mstIFaces = f : mstIFaces s}

splitBy :: Char -> String -> [String]
splitBy delimiter = foldr f [[]] where 
  f c l@(x:xs) | c == delimiter = []:l
               | otherwise = (c:x):xs 
