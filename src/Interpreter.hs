module Interpreter where

import qualified Data.Map as M
import Data.Either (partitionEithers)
import Data.List (foldl', sortOn, maximumBy)
import Data.Function (on)
import Control.Arrow (second)
import Data.Char (isUpper, toLower)

import Types

holeVar :: Var
holeVar = \_ -> Right id
constraintVar :: Ref -> Var
constraintVar r = \r' -> if r == r' then Right id else Left "mismatch"
namedVar :: Symbol -> Var
namedVar name = \r -> Right $ \c -> (name, VRef r) : c

toVar :: Node -> Context -> Var
toVar NHole _ = holeVar
toVar (NSym s) c | Just (VRef r) <- lookup s c = constraintVar r
toVar (NSym s) c | Just _ <- lookup s c = \_ -> Left "non-atomic pattern"
toVar (NSym s) _ = namedVar s

-- Interpreter
update :: Var -> Var -> Context -> Edge
       -> Either String Context
update v1 v2 c (s, t) = do
  f1 <- v1 s
  f2 <- v2 t
  return $ f2 . f1 $ c

one :: [a] -> [a]
one [] = []
one (x : _) = [x]

getStep :: Atom -> Web -> Context -> [Context]
getStep (P s pred t) web c =
  let es = look (map toLower pred) $ edges web
      v1 = toVar s c
      v2 = toVar t c
      (_, cs') = partitionEithers $ map (update v1 v2 c) es
      -- TODO
      cs = if isUpper (head pred) then one cs' else cs'
  in cs

foldStep :: [Symbol] -> [Context] -> [(Context, [Context])]
foldStep names cs = M.toList $ foldl' fold M.empty cs
  where
    fold m c =
      let (key, ctx) = split c
      in M.insertWith (++) key [ctx] m

    split c = split' c ([], [])
    split' [] p = p
    -- requires unique occurrence of names in context!
    split' (b:bs) (l, r)
      | fst b `elem` names = split' bs (l, b:r)
    split' (b:bs) (l, r)   = split' bs (b:l, r)

countStep :: Symbol -> [Context] -> [Context]
countStep s cs =
  let groups = foldStep [s] cs
      fold (c, vals) = (s, VInt (length vals)) : c
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

-- Mutations
fresh :: Web -> (Ref, Web)
fresh (Web c e) = (R c, Web (c+1) e)

newEdge :: (Symbol, Symbol, Symbol) -> Context -> Web -> Web
newEdge (s, pred, t) ctxt web =
  let
    (VRef r1) = look' s ctxt
    (VRef r2) = look' t ctxt
  in
    addEdge pred (r1, r2) web

-- TODO should edges have uids?
delNode :: Ref -> Web -> Web
delNode ref (Web c e) =
    Web c (M.map (filter disjoint) e)
  where
    disjoint (r1, r2) | r1 == ref || r2 == ref = False
    disjoint _ = True

-- RHS operation
stepEff :: (Context, Web) -> Effect -> (Context, Web)
stepEff (c, web) (EFresh name) =
  let (r, web') = fresh web
  in (insertList name (VRef r) c, web')

stepEff (c, web) (EAssert s p t) = (c, newEdge (s,p,t) c web)
stepEff (c, web) (EDel name) | VRef r <- look' name c =
  (c, delNode r web)
