module Run where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, mapMaybe)
import Control.Monad (foldM)

import Debug.Trace (trace)

import Types
import Graph

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

-- Assertion --
assertVal :: SRef -> State -> (SRef, State)
assertVal n s =
  case walk (s_ctxt s) n of
    Left n ->
      let (r, c') = fresh s
      in (Ref r, bind n (Ref r) c')
    Right v -> (v, s)

assert :: State -> Clause -> State
assert s0 (Assert arr) =
  let
  (s, s1) = assertVal (source arr) s0
  (t, s2) = assertVal (target arr) s1
  in addArr (Arrow s (predicate arr) t) s2
assert s0 (Named (App name args)) =
  case M.lookup name (fst s0) of
    Nothing -> error $ "missing definition: " ++ name
    Just (UniquePattern pattern, params) ->
      error "not implemented"
    Just (Pattern pattern, params) ->
      let c' = foldr (uncurry bind) s0 (zip params args)
      in solveAssert c' pattern
assert s0 (All _ _) =
  error "not implemented"


-- TODO unique asserts
solveAssert :: State -> [Clause] -> State
solveAssert s cs = foldl assert s cs

assertCommand :: State -> Pattern -> AssertPattern -> State
assertCommand s0 negative (AssertPattern positive) =
  let env = s_env s0
      ctxt = s_ctxt s0
      matches = solve env negative ctxt
      s1 = foldl (\s c -> solveAssert (m_ctxt c s) positive)
                 s0 matches
  in m_ctxt ctxt s1

commandStep :: State -> Command -> ([String], State)
commandStep s0 (CQuery q) =
  (showResult $ solve (s_env s0) q emptyContext, s0)
  where
    showResult [] = ["[]"]
    showResult xs = map show xs
commandStep s0 (CBinding b@(Binding name _ _)) =
  (["Binding " ++ name], bindP b s0)
commandStep s0 (CAssert p a) = ([], assertCommand s0 p a)

execProgram :: Graph -> Program -> ([String], State)
execProgram g p =
  let s0 = (M.empty, (emptyContext, 0, g))
      step (acc, s0) c =
        let (msgs, s1) = commandStep s0 c in (reverse msgs ++ acc, s1)
      (msgs, s1) = foldl step ([], s0) (commands p)
  in (reverse msgs, s1)
