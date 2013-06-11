module BufTypes where
import Graphics.Rendering.OpenGL hiding (Object, index)
import Data.Sequence

type IBuffers = (IBuffer, IBuffer, IBuffer)
type IBuffer = Seq Int

type VBuffers = (VertexBuffer, TexCoordBuffer, NormalBuffer)
type VertexBuffer = VBuffer (Vertex3 GLfloat)
type TexCoordBuffer = VBuffer (TexCoord2 GLfloat)
type NormalBuffer = VBuffer (Normal3 GLfloat)
type VBuffer a = Seq a

type CBuffers = (CVertexBuffer, CTexCoordBuffer, CNormalBuffer)
type CVertexBuffer = CBuffer (Vertex3 GLfloat)
type CTexCoordBuffer = CBuffer (TexCoord2 GLfloat)
type CNormalBuffer = CBuffer (Normal3 GLfloat)
type CBuffer a = [a]

convVbufToCbuf :: VBuffer a -> CBuffer a
convVbufToCbuf s = case viewl s of 
  EmptyL -> []
  x :< s' -> x : convVbufToCbuf s'

pick :: VBuffer a -> IBuffer -> CBuffer a
pick b is = convVbufToCbuf.fmap (\i -> index b (i-1)) $ is

emptyVbufs :: VBuffers
emptyVbufs = (empty, empty, empty)

emptyIbufs :: IBuffers
emptyIbufs = (empty, empty, empty)

-- bufferAppend :: (Maybe a, Maybe b, Maybe c) -> d -> d
bufferAppend (v, t, n) (vs, ts, ns) = (vs', ts', ns') 
  where vs' = maybe vs (vs|>) v
        ts' = maybe ts (ts|>) t
        ns' = maybe ns (ns|>) n
