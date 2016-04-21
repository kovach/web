module Graph where

import Data.Map (Map)
import qualified Data.Map as M

import Data.Functor.Identity
import Control.Applicative

import Types
import Context as C

addEdge :: Arrow -> Graph -> Graph
addEdge arr (Graph edges) = Graph $
  M.insertWith (++) (predicate arr)
    [(source arr, target arr)] edges

_1 :: Functor f => (a -> f a) -> (a, b) -> f (a, b)
_1 f (a, b) = fmap (\a -> (a,b)) $ f a
_2 :: Functor f => (b -> f b) -> (a, b) -> f (a, b)
_2 f (a, b) = fmap (\b -> (a,b)) $ f b

on l f = runIdentity . l (Identity . f)
get l = getConst . l Const
set l = on l . const
(.~) = get
(.<) = set

type State = ((Module, Context), (Int, Graph))

s_mod :: Functor f => (a -> f a) -> ((a, b), c) -> f ((a, b), c)
s_mod = (_1 . _1)
s_ctxt :: Functor f => (b -> f b) -> ((a, b), c) -> f ((a, b), c)
s_ctxt = (_1 . _2)
s_ctr :: Functor f => (a -> f a) -> (c, (a, b)) -> f (c, (a, b))
s_ctr = (_2 . _1)
s_graph :: Functor f => (b -> f b) -> (c, (a, b)) -> f (c, (a, b))
s_graph = (_2 . _2)

getEnv :: State -> Env
getEnv s = (get s_mod s, get s_graph s)

fresh :: State -> (Ref, State)
fresh s = (R (get s_ctr s), on s_ctr (+1) s)

bind :: Name -> SRef -> State -> State
bind n v = on s_ctxt (insert n v)

bindMod :: Binding -> State -> State
bindMod (Binding name params pattern) s =
  on s_mod (M.insert name (pattern, params)) s

arrow :: Arrow -> State -> State
arrow arr = on s_graph (addEdge arr)

emptyState :: State
emptyState = ((M.empty, C.empty), (0, Graph M.empty))

graphState :: Graph -> State
graphState g = ((M.empty, C.empty), (0, g))
