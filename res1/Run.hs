module Run where

import Types
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, mapMaybe)
import Control.Monad (foldM)

import Debug.Trace (trace)

emptyContext = M.empty

empty [] = True
empty _ = False

type Cursor = (Context, [Clause])

closed :: Cursor -> Bool
closed (_, []) = True
closed _ = False

success cs r = Just $ zip cs (repeat r)

walk :: Context -> SRef -> Either Name SRef
walk c (SName name) =
  case M.lookup name c of
    Nothing          -> Left name
    Just n@(SName _) -> walk c n
    Just r           -> Right r
walk _ x = Right x

unify1 :: SRef -> SRef -> Context -> Maybe Context
unify1 n@(SName _) y c =
  case walk c n of
    Left n -> Just $ M.insert n y c
    Right v -> unify1 v y c
unify1 (SSub _ _) _ _ = error "sub not implemented"
unify1 x y c = if y == x then Just c else Nothing

unify :: [SRef] -> [SRef] -> Context -> Maybe Context
unify a b _ | length a /= length b = error "unify called with invalid lists"
unify as bs c = foldM (\c (a,b) -> unify1 a b c) c (zip as bs)

safeLook k m =
  case M.lookup k m of
    Nothing -> []
    Just es -> es

-- TODO will we ever need access to r?
step :: Env -> Cursor -> Maybe [Cursor]
step env c | closed c = Nothing
step (_, g) (c, Assert arr : r) =
  let es = safeLook (predicate arr) (edges g)
      lhs = [source arr, target arr]
      cs = mapMaybe (\(a,b) -> unify lhs [a,b] c) es
  in success cs r
step env (c, Del refs : r) =
    Just [(foldr delName c refs, r)]
  where
    delName (SName n) m = M.delete n m
    delName _ m = error "del expects name arguments"
step env@(ps, _) (ctxt, Named (App name args) : r) =
  case M.lookup name ps of
    Nothing -> Nothing
    Just (pattern, params) ->
      let bindings = solve env pattern emptyContext
          argVals c = map (fromJust . flip M.lookup c) params
          result = mapMaybe (\binding -> unify args (argVals binding) ctxt) bindings
      in success result r
step env (c, All negative positive : r) =
  let matches = solve env negative c
  in if any empty $ map (solve env positive) matches
     then Nothing
     else Just [(c, r)]
step env (c, SubPattern p : r) =
  success (solve env p c) r

fix :: Env -> [Cursor] -> [Cursor]
fix env cs =
  concatMap (\c -> fix' (c, step env c)) cs
  where
    fix' (c, Nothing) = [c]
    fix' (_, Just cs) = fix env cs

solve :: Env -> Pattern -> Context -> [Context]
solve env (Pattern p) c =
  map fst . filter closed . fix env $ [(c, p)]
solve env (UniquePattern p) c =
  case filter closed . fix env $ [(c, p)] of
    [x] -> [fst x]
    _ -> []
