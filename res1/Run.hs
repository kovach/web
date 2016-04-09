module Run where

import Types
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, mapMaybe)
import Control.Monad (foldM)

emptyContext = M.empty

empty [] = True
empty _ = False

type Cursor = (Context, Pattern)

closed :: Cursor -> Bool
closed (_, []) = True
closed _ = False

many r cs = Just $ zip cs (repeat r)

walk :: Context -> Name -> Either Name SRef
walk c name =
  case M.lookup name c of
    Nothing -> Left name
    Just (SName name') -> walk c name'
    Just r -> Right r

unify1 :: SRef -> SRef -> Context -> Maybe Context
unify1 (SName n) y c =
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

step :: Env -> Cursor -> Maybe [Cursor]
step env c | closed c = Nothing
step (_, g) (c, Assert arr : r) =
  let es = safeLook (predicate arr) (edges g)
      lhs = [source arr, target arr]
      cs = mapMaybe (\(a,b) -> unify lhs [a,b] c) es
  in many r cs
step env (c, Del (SName n) : r) =
  case M.lookup n c of
    Nothing -> Nothing
    Just _ -> Just [(M.delete n c, r)]
step env@(ps, _) (c, Named (App name args) : r) =
  case M.lookup name ps of
    Nothing -> Nothing
    Just (pattern, params) ->
      let cs = map (restrict_context params) (solve env pattern emptyContext)
          argVals c = map (fromJust . flip M.lookup c) params
          result = mapMaybe (\binding -> unify args (argVals binding) c) cs
      in many r result
step env (c, All negative positive : r) =
  let solns = solve env negative c
  in if any empty $ map (solve env positive) solns
     then Nothing
     else Just [(c, r)]

fix :: Env -> [Cursor] -> [Cursor]
fix env cs =
  concatMap (\c -> fix' (c, step env c)) cs
  where
    fix' (c, Nothing) = [c]
    fix' (_, Just cs) = fix env cs

solve :: Env -> Pattern -> Context -> [Context]
solve env p c =
  map fst . filter closed . fix env $ [(c, p)]

restrict_context :: [Name] -> Context -> Context
restrict_context names c =
  M.filterWithKey (\k _ -> k `elem` names) c
