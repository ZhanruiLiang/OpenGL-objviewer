module HalfEdge (
    HEdge(..)
  , HEdgeData(..)
  , HFace(..)
  , HStructure(..)
  , fromIndexSet
  , allVertices
  , makeInterleave
  , expandFace 
  , edgeNeighs
  , vidPair
  )where

import Data.Array.Unboxed 
import Control.Monad
import Foreign (Ptr, newArray)
import qualified Data.Sequence as S 
import Data.Sequence ((|>))
import Data.Maybe
import qualified Data.Map as M

import Graphics.Rendering.OpenGL hiding (index)

import BufTypes
import Utils

data HEdge = HEdge {
    edgeStartV :: !(Vertex3 GLfloat)
  , edgeVid :: !Int
  , edgeData :: !HEdgeData
  , twinE :: Maybe HEdge
  , nextE :: HEdge
  , edgeFace :: HFace
}

instance Show HEdge where
  show e = "E(" ++ show (vidPair e) ++ ", " ++ show (edgeStartV e) ++ ")"

vidPair :: HEdge -> (Int, Int)
vidPair e = ((edgeVid e), edgeVid (nextE e))
-- vidPair e = if (edgeVid.fromJust.twinE$ e) /= (edgeVid.nextE$ e) then error "xxx" else ((edgeVid e), edgeVid (nextE e))

data HEdgeData = HEdgeData {
    vtxTexcoord :: Maybe (TexCoord2 GLfloat)
  , vtxNormal :: Maybe (Normal3 GLfloat)
}

data HFace = HFace {
    faceEdge :: HEdge
}

instance Show HFace where
  show f = show (expandFace f)

data HStructure = HStructure {
    hsHead :: HEdge
  , hsFaces :: [HFace]
}

instance Show HStructure where
  show (HStructure e0 fs) = "HStructure(" ++ show e0 ++ ", " ++ ppshow fs ++ ")"

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

expandFace :: HFace -> [HEdge]
expandFace f = take (3) $ iterate nextE (faceEdge f)

allVertices :: HStructure -> [Vertex3 GLfloat]
allVertices (HStructure _ fs) = map edgeStartV . concat .map expandFace $ fs

-- Find the neighbours of a vertex given a HalfEdge
edgeNeighs :: HEdge -> [Vertex3 GLfloat]
edgeNeighs e = travel (edgeVid.nextE$ e) (twinE.nextE.fromJust.twinE$e) where
  -- | travel v1 e: travel from e, end at an edge that starts with vid v1
  travel :: Int -> Maybe HEdge -> [Vertex3 GLfloat]
  travel v1   Nothing = []
  travel v1 (Just e) = if edgeVid e == v1 then [edgeStartV e]
        else edgeStartV e : travel v1 (twinE.nextE$ e)

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
