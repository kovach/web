module Interpreter where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Either (rights)
import Data.List (foldl', sortOn, maximumBy, partition, nub)
import Data.Maybe (mapMaybe)
import Data.Function (on)
import Control.Arrow (second)
import Data.Char (isUpper, toLower)

import Types
import Extern (std_lib, matchExtern)
import Sub

holeVar :: Var
holeVar = \_ -> Right id
constraintVar :: Expr -> Var
constraintVar r = \r' -> if r == r' then Right id else Left "mismatch"
namedVar :: Symbol -> Var
namedVar name = \r -> Right $ \c -> (name, r) : c

toVar :: Expr -> Context -> Var
--toVar NHole _ = holeVar
toVar (ESym s) c | Just r <- lookup s c = constraintVar r
--toVar (NRoot s) c | Just r <- lookup s c = constraintVar r
toVar (ESym s) _ = namedVar s
--toVar (NRoot s) _ = namedVar s
toVar (ELit lit) _ = constraintVar $ ELit lit

-- Interpreter
update :: Expr -> Expr -> Context -> Edge
       -> Either String Context
update v1 v2 c (s, t) = do
  f1 <- toVar v1 c s
  f2 <- toVar v2 c t
  return $ f2 . f1 $ c

--TODO remove
--isRooted (NRoot base) (NSym prop) = Just (base, prop)
--isRooted (NSym prop) (NRoot base) = Just (base, prop)
isRooted _ _ = Nothing

getStep :: Arrow -> Graph -> Context -> [Context]
getStep (Arrow s pred t) (Graph g) c =
  case isRooted s t of
    --Just (base, prop) -> map takeOne $ foldStep [prop] newContexts
    Nothing -> newContexts
  where
    -- ? TODO is nub okay
    newContexts = nub $ rights $ map (update s t c) $ look pred g
    -- NB this reverses the order of binding
    -- in the case of (a pred !b)
    takeOne (c, cs) = head cs ++ c

foldStep :: [Symbol] -> [Context] -> [(Context, [Context])]
foldStep names cs = M.toList $ foldr fold M.empty cs
  where
    fold c m =
      let (key, ctx) = split c
      in M.insertWith (++) key [ctx] m

    split c = partition (not . (`elem` names) . fst) c

countStep :: Symbol -> [Context] -> [Context]
countStep s cs =
  let groups = foldStep [s] cs
      fold (c, vals) = (s, ELit (LInt (length vals))) : c
  in map fold groups

maxStep :: Max -> [Context] -> [Context]
maxStep (Max base val) cs =
  let groups = foldStep [base, val] cs
      fold (c, pairs) =
        let bval = look' base $
              maximumBy (compare `on` (look' val)) pairs
        in (base, bval) : c
  in map fold groups

dropStep :: Symbol -> [Context] -> [Context]
dropStep s cs =
  let groups = foldStep [s] cs
      fold (c, _) = c
  in map fold groups

-- LHS operation
step :: Graph -> [Context] -> Effect -> [Context]
step w cs (Assert a) = concatMap (getStep a w) cs
--step w cs (OCount s) = countStep s cs
--step w cs (OMax m) = maxStep m cs
--step w cs (ODrop s) = dropStep s cs
step w cs (ENamed (App sym args)) = concat $ mapMaybe (\c ->
  matchExtern w c args (look' sym std_lib)) cs

-- Mutations
fresh :: Log -> (Ref, Log)
fresh (Log c e) = (R c, Log (c+1) e)

e2v :: (Context, Log) -> Expr -> (Expr, (Context, Log))
e2v p (ELit l) = (ELit l, p)
e2v p (ERef r) = (ERef r, p)
e2v p@(ctxt, w0) (ESym name) =
  case lookup name ctxt of
    Just v -> (v, p)
    Nothing ->
      let (r, w1) = fresh w0
      in (ERef r, ((name, ERef r) : ctxt, w1))

addEdge :: Symbol -> Edge -> Graph -> Graph
addEdge pred e (Graph env) = Graph (M.insertWith (++) pred [e] env)

--newEdge :: Arrow -> Context -> Graph -> (Context, Graph)
--newEdge (Arrow s pred t) c0 w0 =
--  let
--    (v1, p1) = e2v (c0, w0) s
--    (v2, (c2, w2)) = e2v p1 t
--  in
--    (c2, addEdge pred (v1, v2) w2)

-- TODO should edges have uids?
delNode :: Ref -> Graph -> Graph
delNode ref (Graph e) =
    Graph (M.map (filter disjoint) e)
  where
    disjoint (r1, r2) | r1 == ERef ref || r2 == ERef ref = False
    disjoint _ = True

canonicalize :: Effect -> (Context, Log) -> RuleContext -> (Effect, (Context, Log))
canonicalize (Assert (Arrow s pred t)) p0 _ =
  let
    (v1, p1) = e2v p0 s
    (v2, p2) = e2v p1 t
  in (Assert (Arrow v1 pred v2), p2)
canonicalize (Del e) p0 _ =
  let (v, p) = e2v p0 e in (Del v, p)
canonicalize (ENamed app) p rc =
  let (es, _) = normalize rc ([ENamed app], [])
  in error "canonicalize: enamed"

-- RHS operation
stepEff :: (Context, Graph) -> Effect -> (Context, Graph)
--stepEff (c, web) (EFresh name) =
--  let (r, web') = fresh web
--  in ((name, VRef r) : c, web')
--stepEff (c, web) (Assert arr) = newEdge arr c web
stepEff (c, web) (Del (ESym name)) | ERef r <- look' name c =
  (c, delNode r web)
stepEff (c, web) (Del (ERef r)) =
  (c, delNode r web)
