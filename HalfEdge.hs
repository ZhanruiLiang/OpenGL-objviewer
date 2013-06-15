module HalfEdge (
    HEdge(..)
  , HEdgeData(..)
  , HFace(..)
  , HStructure(..)
  , fromIndexSet
  , renderHStruture
  , allVertices
  , makeInterleave
  )where
import qualified Data.Map as M
import Data.Array.Unboxed 
import qualified Data.Sequence as S 
import Data.Sequence ((|>))
import Control.Monad
import Data.Maybe
import Foreign (Ptr, newArray)
import Graphics.Rendering.OpenGL hiding (index)

import BufTypes
import Utils

data HEdge = HEdge {
    edgeStartV :: Vertex3 GLfloat
  , edgeVid :: Int
  , edgeData :: HEdgeData
  , twinE :: Maybe HEdge
  , nextE :: HEdge
  , edgeFace :: HFace
}

vidPair :: HEdge -> (Int, Int)
vidPair e = ((edgeVid e), edgeVid (nextE e))

data HEdgeData = HEdgeData {
    vtxTexcoord :: Maybe (TexCoord2 GLfloat)
  , vtxNormal :: Maybe (Normal3 GLfloat)
}

data HFace = HFace {
    faceEdge :: HEdge
}

data HStructure = HStructure {
    hsHead :: HEdge
  , hsFaces :: [HFace]
}


mkArray :: [a] -> Array Int a
mkArray xs = listArray (0, length xs-1) xs

(!?) :: Array Int a -> Maybe Int -> Maybe a
a !? mi = (a !) `liftM` mi

fromIndexSet :: VBuffers -> [IFace] -> HStructure
fromIndexSet vbufs ifaces = let
  (vs, ts, ns) = vbufs
  vs' = mkArray (convVbufToCbuf vs)
  ts' = mkArray (convVbufToCbuf ts)
  ns' = mkArray (convVbufToCbuf ns)
  vs'' = remap $ map (\(i, _, _) -> i).concat$ ifaces

  esMap = M.fromList esList
  (esList', faces') = unzip $ map makeSubList ifaces
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
        , edgeVid = fromJust $ M.lookup ix1 vs'' 
        , edgeData = HEdgeData (ts'!?ti) (ns'!?ni)
        , twinE = M.lookup (ix2, ix1) esMap
        , nextE = snd $ es !! ((i+1)`mod`m)
        , edgeFace = hface
      }
  in HStructure (snd$head esList) faces'

remap :: [Int] -> M.Map Int Int
remap a = process 0 a M.empty  where
  process _ [] a' = a'
  process i (x:xs) a' = case M.lookup x a' of
    Nothing -> process (i+1) xs (M.insert x i a')
    Just _  -> process i xs a'

makeInterleave :: [HFace] -> IO (GLsizei, Ptr GLfloat)
makeInterleave faces = newArray a >>= (\a -> return (n, a))
  where
    a = concatMap expandVertex . concatMap expandFace $ faces 
    n = fromIntegral $ length a `div` 8 
    expandVertex :: HEdge -> [GLfloat]
    -- | expand to T2FN3FV3F
    expandVertex e = [tx, ty, nx, ny, nz, vx, vy, vz] where
      (Vertex3 vx vy vz) = edgeStartV e
      (TexCoord2 tx ty) = filt2 . vtxTexcoord . edgeData $ e
      (Normal3 nx ny nz) = filt3 . vtxNormal . edgeData $ e
      filt2 Nothing = (TexCoord2 0 0)
      filt2 (Just x) = x
      filt3 Nothing = (Normal3 1 0 0)
      filt3 (Just x) = x

renderHStruture (HStructure _ fs) = do
  renderPrimitive Triangles $ do
    forM_ (concat.map expandFace $fs) (\e-> do
        let t = vtxTexcoord.edgeData $ e
            n = vtxNormal.edgeData $ e
        if isJust t then 
          texCoord $ fromJust t
          else return ()
        if isJust n then 
          normal $ fromJust n 
          else return ()
        vertex.edgeStartV $ e)

expandFace :: HFace -> [HEdge]
expandFace f = take (3) $ iterate nextE (faceEdge f)

allVertices (HStructure _ fs) = map edgeStartV . concat .map expandFace $ fs

-- loopSubdiv (HStructure e0 fs) = HStructure e0' fs' where
  --TODO


-- | Test cases ---------------------------------------------------------------
-------------------------------------------------------------------------------

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
