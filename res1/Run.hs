module Run where

import Types
import Data.Map (Map)
import qualified Data.Map as M

empty [] = True
empty _ = False

type Cursor = (Context, Pattern)

closed :: Cursor -> Bool
closed (_, []) = True
closed _ = False

step :: Env -> Cursor -> Maybe [Cursor]
step env c | closed c = Nothing
step (_, g) (c, Assert arr : r) = _
step env (c, Del (SName n) : r) =
  case M.lookup n c of
    Nothing -> Nothing
    Just _ -> Just [(M.delete n c, r)]
step env@(ps, _) (c, Named (App name args) : r) =
  case M.lookup name ps of
    Nothing -> Nothing
    Just p ->
      let cs = solve env (lift_pattern p args) c
      in Just $ zip cs (repeat r)
step env (c, All negative positive : r) =
  let solns = solve env negative c
  in if any empty $ map (solve env positive) solns
     then Nothing
     else Just [(c, r)]

fix :: Env -> [Cursor] -> [Cursor]
fix env cs =
  let cs' = map (\c -> (c, step env c)) cs
  in concatMap fix' cs'
  where
    fix' (c, Nothing) = [c]
    fix' (_, Just cs) = fix env cs

solve :: Env -> Pattern -> Context -> [Context]
solve env p c = map fst . filter closed . fix env $ [(c, p)]

walk :: Context -> Name -> Either Name SRef
walk c name =
  case M.lookup name c of
    Nothing -> Left name
    Just name' -> walk c name'

-- just solve pattern
-- return context with only params
-- standard arrow unification between params/args
lift_pattern :: (Pattern, [Name]) -> [SRef] -> Pattern
lift_pattern (p, params) args = _
