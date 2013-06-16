module HalfEdge (
    HEdge(..)
  , HEdgeData(..)
  , HFace(..)
  , HStructure(..)
  , fromIndexSet
  , allVertices
  , makeInterleave
  , loopSubdiv
  , edgeNeighs
  , loopNewPos
  , loopMidNewPos
  )where
import qualified Data.Map as M
import Data.Array.Unboxed 
import Data.List
import Data.Function
import qualified Data.Sequence as S 
import Data.Sequence ((|>))
import Control.Monad
import Data.Maybe
import Foreign (Ptr, newArray)
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

(Vertex3 x y z) !+! (Vertex3 x' y' z') = Vertex3 (x + x') (y + y') (z + z')
k !*! (Vertex3 x y z) = Vertex3 (k*x) (k*y) (k*z)

infixl 6 !+!
infixl 7 !*!

loopNewPos :: Vertex3 GLfloat -> [Vertex3 GLfloat] -> Vertex3 GLfloat
loopNewPos v0 vs = (1-n * b) !*! v0 !+! foldl1' (!+!) (map (b !*!) vs) where
  n = fromIntegral $ length vs
  sqr x = x * x
  b = 1.0 / n * (5/8 - sqr (3/8 + 1/4 * cos (2 * pi / n)))
-- loopNewPos v0 _ = v0

loopMidNewPos :: HEdge -> Vertex3 GLfloat
{-
  - v1 <------- v3       w1 =   
  -  \ \ _        ^      w2 = 3/4 - w1
  -   \   ->m_     \     w3 = 1/8    
  -    \      \-->  \    w4 = 1/8    
  -     v4 --------> v2              
  -  e = (v1 --> v2)                    
  -}
loopMidNewPos e = let 
    v1 = edgeStartV e
    v2 = edgeStartV.nextE$ e
    v3 = edgeStartV.nextE.nextE $ e
    v4 = edgeStartV.nextE.nextE.fromJust.twinE $ e
    n = fromIntegral . length $ edgeNeighs e
    w1 = 3/4 - w2
    -- w2 = 0.5 - 0.25 * cos (2 * pi / (n - 1))
    w2 = 3/8
    w3 = 1/8
    w4 = w3
  in w1 !*! v1 !+! w2 !*! v2 !+! w3 !*! v3 !+! w4 !*! v4 
  -- in 0.5 !*! v1 !+! 0.5 !*! v2


-- emidmap: (vid1, vid2) -> midv
makeEmidmap :: [HFace] -> M.Map (Int, Int) (Vertex3 GLfloat)
makeEmidmap fs = emidmap where
  emidmap = M.fromList $ map makeMid (concatMap expandFace fs)
  makeMid e = ((i, j), vmid) where
    (i, j) = vidPair e
    vmid = if i <= j then loopMidNewPos e
           else fromJust$ M.lookup (j, i) emidmap 

loopSubdiv :: HStructure -> HStructure
--   Eace face subdivide into o4 faces     
--               p3                       
--             /     ^                    
--           /        \                   
--          v--------->\                  
--       p6 <----------  p5               
--      / ^\        ^ /    ^              
--     /   \\      //       \             
--    v     \v   / v         \            
--   p1 -----> p4  ----------> p2         
--                                        
loopSubdiv (HStructure e0 fs) = HStructure e0' fs'' where
  -- startEdges: Mapping from vid to edge
  startEdges :: M.Map Int HEdge
  startEdges = id `seq` M.fromList $ map (\e -> (edgeVid e, e)) . concatMap expandFace$ fs
  -- n1: number of vertices before subdivision
  n1 = M.size startEdges
  vs1 = map ((\e -> loopNewPos (edgeStartV e) (edgeNeighs e)). (fromJust.flip M.lookup startEdges)) [0..n1-1]
  -- emidmap': (vid1, vid2) -> midvid
  emidmap' :: M.Map (Int, Int) (Int, Vertex3 GLfloat)
  (n, emidmap') = M.foldlWithKey' makeMid' (n1, M.empty) (makeEmidmap fs)
  -- makeMid' : take (curID, curMap) (vid1, vid2) curMidVid
  makeMid' (t, m) (i, j) v = if i <= j 
    then (t+1, M.insert (i, j) (t, v) m)
    else (t  , M.insert (i, j) (fromJust$ M.lookup (j, i) emidmap') m)
  -- n - n1: number of new created vertices
  vs2 :: [Vertex3 GLfloat]
  vs2 = half.snd.unzip.sortBy (compare `on` fst)$ M.elems emidmap'
  half [] = []
  half (a:b:xs) = a:half xs
  -- all vertices after subdivision
  vs' :: Array Int (Vertex3 GLfloat)
  vs' = id `seq` listArray (0, n - 1)$ vs1 ++ vs2
  -- esmap : (i, j) -> edge
  esmap :: M.Map (Int, Int) HEdge
  esmap = M.fromList (concat eslist)
  eslist :: [[((Int, Int), HEdge)]]
  (eslist, fs') = unzip.map makeSubList$ fs
  fs'' = concat fs'
  e0' = head (M.elems esmap)
  -- makeSubList: subdivide a face into 4 subfaces
  makeSubList :: HFace -> ([((Int, Int), HEdge)], [HFace])
  makeSubList face = (es', fs) where
    (es, fs) = unzip.map newFace$ [
        (p1, p4, p6), (p4, p2, p5), (p5, p3, p6), (p4, p5, p6)]
    es' = concat es
    [p1, p2, p3] = map edgeVid [e1, e2, e3]
    [e1, e2, e3] = take 3 $ iterate nextE (faceEdge face)
    [p4, p5, p6] = map (\(i,j)->mid i j) [(p1, p2), (p2, p3), (p3, p1)]

  mid i j = fst . fromJust . M.lookup (i, j)$ emidmap'
  find i j = M.lookup (i, j) esmap
  emptyEdgeData = HEdgeData Nothing Nothing
  newE i j next f = HEdge {
      edgeStartV = (vs'!i)
    , edgeVid = i
    , edgeData = emptyEdgeData
    , nextE = next
    , twinE = find j i
    , edgeFace = f }
  newFace (i, j, k) = ([e1', e2', e3'], f) where
    e1 = newE i j e2 f
    e2 = newE j k e3 f
    e3 = newE k i e1 f
    e1' = ((i, j), e1)
    e2' = ((j, k), e2)
    e3' = ((k, i), e3)
    f = HFace e1

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
