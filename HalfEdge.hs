module HalfEdge (
    HEdge(..)
  , HEdgeData(..)
  , HFace(..)
  , HStructure(..)
  , fromIndexSet
  , renderHStruture
  , allVertices
  )where
import qualified Data.Map as M
import Data.Array.Unboxed hiding (length)
import Data.Sequence as S hiding (length)
import Control.Monad
import Data.Maybe
import Graphics.Rendering.OpenGL hiding (Object, index)

import BufTypes

data HEdge = HEdge {
    edgeStartV :: Vertex3 GLfloat
  , edgeData :: HEdgeData
  , twinE :: Maybe HEdge
  , nextE :: HEdge
  , edgeFace :: HFace
}

data HEdgeData = HEdgeData {
    vtxTexcoord :: Maybe (TexCoord2 GLfloat)
  , vtxNormal :: Maybe (Normal3 GLfloat)
}

data HFace = HFace {
    faceEdge :: HEdge
}

data HStructure = HStructure {
    stHead :: HEdge
  , stFaces :: [HFace]
}


mkArray :: [a] -> Array Int a
mkArray xs = listArray (0, length xs-1) xs

fromIndexSet :: VBuffers -> [IFace] -> HStructure
fromIndexSet vbufs faces = let
    (vs, ts, ns) = vbufs
    vs' = mkArray (convVbufToCbuf vs)
    ts' = mkArray (convVbufToCbuf ts)
    ns' = mkArray (convVbufToCbuf ns)

    esMap = M.fromList esList
    (esList', faces') = unzip $ map makeSubList faces
    esList = concat esList'
    makeSubList :: IFace -> ([((Int, Int), HEdge)], HFace)
    makeSubList f = (es, hface) where
        m = length f
        es = map makeHEdge [0..m - 1]
        hface = HFace (snd$ head es)
        makeHEdge i = ((ix1, ix2), e) where
          (ix1, ti, ni) = f!!i
          (ix2, _, _)  = f!!((i+1)`mod`m)
          e = HEdge {
              edgeStartV = vs'!ix1
            , edgeData = HEdgeData (ts'!?ti) (ns'!?ni)
            , twinE = M.lookup (ix2, ix1) esMap
            , nextE = snd $ es !! ((i+1)`mod`m)
            , edgeFace = hface
          }
  in HStructure (snd$head esList) faces'

(!?) :: Array Int a -> Maybe Int -> Maybe a
a !? mi = (a !) `liftM` mi

renderHStruture (HStructure _ fs) = do
  renderPrimitive Triangles $ do
    forM_ (concat.map expand.map faceEdge$fs) (\e-> do
        let t = vtxTexcoord.edgeData $ e
            n = vtxNormal.edgeData $ e
        if isJust t then texCoord $ fromJust t else return ()
        if isJust n then normal $ fromJust n else return ()
        vertex.edgeStartV $ e)

expand :: HEdge -> [HEdge]
expand e = [e, nextE e, nextE (nextE e)]

allVertices (HStructure _ fs) = map edgeStartV . concat .map (expand.faceEdge)$ fs

test = let
  v3f x y z = Vertex3 (x::GLfloat) y z
  v1 = v3f 0 0 0
  v2 = v3f 1 0 0
  v3 = v3f 0 1 0
  v4 = v3f 0 0 1
  vbufs = (
        S.empty |> v1 |> v2 |> v3 |> v4
      , S.empty 
      , S.empty
      )
  i3 i = (i, Nothing, Nothing)
  faces = [
    [i3 1, i3 2, i3 4],
    [i3 2, i3 3, i3 4],
    [i3 1, i3 4, i3 3],
    [i3 2, i3 1, i3 3]]
  in fromIndexSet vbufs faces

main = do
  let HStructure es fs = test
  putStrLn.show$ length fs
