module ObjRead (
    loadModel
  , Object(..)
  , Model(..)
  , Material(..)
  )where

import Data.Maybe
import Control.Monad
import System.FilePath
import Foreign (Ptr, newArray, nullPtr)
import qualified Data.Array.IArray as IA
import qualified Data.Map.Lazy as M
import qualified Data.Sequence as S
import Graphics.Rendering.OpenGL hiding (Object)
import Graphics.UI.GLFW as GLFW

import BufTypes
import Utils

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
  GLFW.loadTexture2D path [BuildMipMaps]
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
        otherwise -> return (mtl, lib)

-- | Object data type
data Object = Object {
    objName :: String
  , objBuffers :: CBuffers
  , objMaterial :: Maybe Material
} deriving (Show)

-- | Model data type
type Model = [Object]

addObject :: Object -> Model -> Model
addObject obj = (obj:)

getObject :: String -> Model -> Object
getObject name = head.filter ((name==).objName)

emptyModel = []

data ModelState = ModelState {
    mstVBuffers :: VBuffers
  , mstIBuffers :: IBuffers
  , mstMtllib :: Maybe Mtllib
  , mstMaterial :: Maybe Material
  , mstObjName :: String
  , mstModel :: Model
}

emptyMst = ModelState {
    mstVBuffers = emptyVbufs
  , mstIBuffers = emptyIbufs
  , mstMtllib = Nothing
  , mstMaterial = Nothing
  , mstObjName = ""
  , mstModel = emptyModel
}

loadModel :: FilePath -> IO Model
loadModel path = do
    datas <- readFile path
    s <- foldM parseLine emptyMst (lines datas)
    return $ mstModel (addObject' True s)
  where 
    addObject' allowNull s = let
        model = mstModel s
        model' = if allowNull || (not.null.mstObjName$ s) then addObject obj model
                 else model
        obj = Object {
            objName = mstObjName s
          , objBuffers = makeObjBuffer (mstVBuffers s) (mstIBuffers s)
          , objMaterial = mstMaterial s
        }
      in s { 
            mstIBuffers = emptyIbufs
          , mstModel = model'
        }
    parseLine :: ModelState -> String -> IO ModelState
    parseLine s line = let
      (cmd, args) = (head toks, tail toks)
      toks = words line
      mtllib = fromJust$ mstMtllib s
      in case cmd of
        -- o objName
        "o" -> return $ (addObject' False s) { mstObjName = head args }
        "g" -> return $ (addObject' False s) { mstObjName = head args }
        -- usemtl mtlName
        "usemtl" -> return s { mstMaterial = lookupMaterial mtllib (args!!0) }
        -- v x y z
        "v" -> let [x, y, z] = map read $ take 3 args
          in return $ appendVbuf (Just$Vertex3 x y z, Nothing, Nothing) s 
        -- vn nx ny nz
        "vn" -> let [x, y, z] = map read $take 3 args
          in return $ appendVbuf (Nothing, Nothing, Just$Normal3 x y z) s 
        -- vt tx ty
        "vt" -> let [x, y] = map read $ take 2 args
          in return $ appendVbuf (Nothing, Just$TexCoord2 x y, Nothing) s 
        -- mtllib libName
        "mtllib" -> do
          let dir = fst.splitFileName $ path
          mtllib <- loadMtllib (joinPath [dir, args!!0])
          return $ s { mstMtllib = Just mtllib }
        -- f i1 i2 i3 # triangle faces
        "f" | length args == 3 -> 
                return$ foldl (\s g->appendIbuf (conv g) s) s (map (splitBy '/') args)
            | otherwise -> error "Not a triangular face"
              where 
                conv [v, t, n] = (sconv v, sconv t, sconv n)
                conv g = error $ "invalid index tuple number:" ++ show g
                sconv "" = Nothing 
                sconv s = Just $ (read s :: Int)
        otherwise -> return s
    appendVbuf x s = s { mstVBuffers = bufferAppend x (mstVBuffers s) }
    appendIbuf x s = s { mstIBuffers = bufferAppend x (mstIBuffers s) }

splitBy :: Char -> String -> [String]
splitBy delimiter = foldr f [[]] where 
  f c l@(x:xs) | c == delimiter = []:l
               | otherwise = (c:x):xs 

makeObjBuffer :: VBuffers -> IBuffers -> CBuffers
makeObjBuffer (vs0, ts0, ns0) (vi, ti, ni) = (vs, ts, ns) 
  where vs = pick vs0 vi
        ts = pick ts0 ti
        ns = pick ns0 ni
