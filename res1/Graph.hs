module Graph where

import Data.Map (Map)
import qualified Data.Map as M

import Types

addEdge :: Arrow -> Graph -> Graph
addEdge arr (Graph edges) = Graph $
  M.insertWith (++) (predicate arr)
    [(source arr, target arr)] edges

-- (Immutable, Mutable)
-- TODO lenses
type State = (Module, (Context, Int, Graph))
s_ctxt (_, (c, _, _)) = c
m_ctxt c (a, (_, b, d)) = (a, (c,b,d))
s_ctr  (_, (_, c, _)) = c
s_env  (m, (_, _, g)) = (m, g)
fresh :: State -> (Ref, State)
fresh (a, (b, c, d)) = (R c, (a, (b, c+1, d)))
bind :: Name -> SRef -> State -> State
bind n v (a, (b, c, d)) = (a, (M.insert n v b, c, d))
bindP :: Binding -> State -> State
bindP (Binding name params pattern) (a, t) =
  (M.insert name (pattern, params) a, t)
addArr :: Arrow -> State -> State
addArr arr (a, (b, c, g)) = (a, (b, c, addEdge arr g))
emptyState :: State
emptyState = (M.empty, (emptyContext, 0, Graph M.empty))
s_graph (_, (_,_,g)) = g
-- ODOT

