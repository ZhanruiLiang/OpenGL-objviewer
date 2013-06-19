module LoopSubdiv (
    loopSubdiv
  , loopNewPos
  , loopMidNewPos
    ) where
import HalfEdge

import Data.Array.Unboxed 
import Data.List
import Data.Function
import qualified Data.Sequence as S 
import Data.Sequence ((|>))
import Data.Maybe
import qualified Data.Map as M

import Graphics.Rendering.OpenGL hiding (index)

(Vertex3 x y z) !+! (Vertex3 x' y' z') = Vertex3 (x + x') (y + y') (z + z')
k !*! (Vertex3 x y z) = Vertex3 (k*x) (k*y) (k*z)

infixl 6 !+!
infixl 7 !*!

loopNewPos :: Vertex3 GLfloat -> [Vertex3 GLfloat] -> Vertex3 GLfloat
loopNewPos v0 vs = id `seq` (1-n * b) !*! v0 !+! foldl1' (!+!) (map (b !*!) vs) where
  n = fromIntegral $ length vs
  sqr x = x * x
  b = 1.0 / n * (5/8 - sqr (3/8 + 1/4 * cos (2 * pi / n)))
-- loopNewPos v0 _ = v0

loopMidNewPos :: HEdge -> (HEdge -> Vertex3 GLfloat) -> Vertex3 GLfloat
{-
  - v1 <------- v3       w1 =   
  -  \ \ _        ^      w2 = 3/4 - w1
  -   \   ->m_     \     w3 = 1/8    
  -    \      \-->  \    w4 = 1/8    
  -     v4 --------> v2              
  -  e = (v1 --> v2)                    
  -}
loopMidNewPos e g = let 
    v1 = g e
    v2 = g.nextE$ e
    v3 = g.nextE.nextE $ e
    v4 = g.nextE.nextE.fromJust.twinE $ e
    n = fromIntegral . length $ edgeNeighs e
    w1 = 3/4 - w2
    -- w2 = 0.5 - 0.25 * cos (2 * pi / (n - 1))
    w2 = 3/8
    w3 = 1/8
    w4 = w3
    w3' = 1 / 4
  in if isJust.twinE $ e 
     then w1 !*! v1 !+! w2 !*! v2 !+! w3 !*! v3 !+! w4 !*! v4 
     else w1 !*! v1 !+! w2 !*! v2 !+! w3' !*! v3
  -- in 0.5 !*! v1 !+! 0.5 !*! v2


-- emidmap: (vid1, vid2) -> midv
makeEmidmap :: [HFace] -> (HEdge -> Vertex3 GLfloat)  -> M.Map (Int, Int) (Vertex3 GLfloat)
makeEmidmap fs g = emidmap where
  emidmap = M.fromList $ map makeMid (concatMap expandFace fs)
  makeMid e = ((i, j), vmid) where
    (i, j) = vidPair e
    vmid = if (isNothing.twinE$ e) || i <= j then loopMidNewPos e g
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
  startEdges = seq id . M.fromList . map (\e -> (edgeVid e, e)) . concatMap expandFace$ fs
  -- n1: number of vertices before subdivision
  n1 = M.size startEdges
  vs1 = map ((\e -> loopNewPos (edgeStartV e) (edgeNeighs e)). (fromJust.flip M.lookup startEdges)) [0..n1-1]
  -- emidmap = id `seq` makeEmidmap fs (\e -> vs'!(edgeVid e))
  emidmap = id `seq` makeEmidmap fs edgeStartV
  -- emidmap': (vid1, vid2) -> midvid
  emidmap' :: M.Map (Int, Int) (Int, Vertex3 GLfloat)
  (n, emidmap') = M.foldlWithKey' makeMid' (n1, M.empty) (emidmap)
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
  vs' = listArray (0, n - 1)$ vs1 ++ vs2
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

