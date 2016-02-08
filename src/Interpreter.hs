module Interpreter where

import qualified Data.Map as M
import Data.Either (rights)
import Data.List (foldl', sortOn, maximumBy, partition, nub)
import Data.Maybe (mapMaybe)
import Data.Function (on)
import Control.Arrow (second)
import Data.Char (isUpper, toLower)

import Types
import Extern (std_lib, matchExtern)

holeVar :: Var
holeVar = \_ -> Right id
constraintVar :: Value -> Var
constraintVar r = \r' -> if r == r' then Right id else Left "mismatch"
namedVar :: Symbol -> Var
namedVar name = \r -> Right $ \c -> (name, r) : c

toVar :: Node -> Context -> Var
toVar NHole _ = holeVar
toVar (NSym s) c | Just r <- lookup s c = constraintVar r
toVar (NRoot s) c | Just r <- lookup s c = constraintVar r
toVar (NSym s) _ = namedVar s
toVar (NRoot s) _ = namedVar s
toVar (NLit lit) _ = constraintVar $ VLit lit

-- Interpreter
update :: Node -> Node -> Context -> Edge
       -> Either String Context
update v1 v2 c (s, t) = do
  f1 <- toVar v1 c s
  f2 <- toVar v2 c t
  return $ f2 . f1 $ c

isRooted (NRoot base) (NSym prop) = Just (base, prop)
isRooted (NSym prop) (NRoot base) = Just (base, prop)
isRooted _ _ = Nothing

getStep :: Atom -> Web -> Context -> [Context]
getStep (Atom s pred t) web c =
  case isRooted s t of
    Just (base, prop) -> map takeOne $ foldStep [prop] newContexts
    Nothing -> newContexts
  where
    -- ? TODO is nub okay
    newContexts = nub $ rights $ map (update s t c) $ look pred $ edges web
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
      fold (c, vals) = (s, VLit (LInt (length vals))) : c
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
step :: Web -> [Context] -> Operation -> [Context]
step w cs (OMatch p) = concatMap (getStep p w) cs
step w cs (OCount s) = countStep s cs
step w cs (OMax m) = maxStep m cs
step w cs (ODrop s) = dropStep s cs
step w cs (OExtern (App sym args)) = concat $ mapMaybe (\c ->
  matchExtern w c args (look' sym std_lib)) cs

-- Mutations
fresh :: Web -> (Ref, Web)
fresh (Web c e) = (R c, Web (c+1) e)

token2val :: (Context, Web) -> VE -> (Value, (Context, Web))
token2val p (VELit l) = (VLit l, p)
token2val p (VERef r) = (VRef r, p)
token2val p@(ctxt, w0) (VESym name) =
  case lookup name ctxt of
    Just v -> (v, p)
    Nothing ->
      let (r, w1) = fresh w0
      in (VRef r, ((name, VRef r) : ctxt, w1))

newEdge :: Arrow -> Context -> Web -> (Context, Web)
newEdge (Arrow s pred t) c0 w0 =
  let
    (v1, p1) = token2val (c0, w0) s
    (v2, (c2, w2)) = token2val p1 t
  in
    (c2, addEdge pred (v1, v2) w2)

-- TODO should edges have uids?
delNode :: Ref -> Web -> Web
delNode ref (Web c e) =
    Web c (M.map (filter disjoint) e)
  where
    disjoint (r1, r2) | r1 == VRef ref || r2 == VRef ref = False
    disjoint _ = True

-- RHS operation
stepEff :: (Context, Web) -> Effect -> (Context, Web)
--stepEff (c, web) (EFresh name) =
--  let (r, web') = fresh web
--  in ((name, VRef r) : c, web')
stepEff (c, web) (EAssert s p t) = newEdge (Arrow (e2ve s) p (e2ve t)) c web
stepEff (c, web) (EDel name) | VRef r <- look' name c =
  (c, delNode r web)
