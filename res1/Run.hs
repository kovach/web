module Run where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, mapMaybe)
import Control.Monad (foldM)

import Debug.Trace (trace)

import Types
import qualified Context as C

import Graph (State, (.~), (.<))
import qualified Graph as G

empty [] = True
empty _ = False

type Cursor = (Context, [Clause])

closed :: Cursor -> Bool
closed (_, []) = True
closed _ = False

success cs r = Just $ zip cs (repeat r)

unify1 :: SRef -> SRef -> Context -> Maybe Context
unify1 n@(SName _) y c =
  case C.walk c n of
    Left n -> Just $ C.insert n y c
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
    delName (SName n) m = C.delete n m
    delName _ m = error "del expects name arguments"
step env@(ps, _) (ctxt, Named (App name args) : r) =
  case M.lookup name ps of
    Nothing -> Nothing
    Just (pattern, params) ->
      let bindings = solve env pattern C.empty
          argVals c = map (fromJust . flip C.lookup c) params
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
-- TODO make fresh monadic
assertVal :: SRef -> State -> (SRef, State)
assertVal n s =
  let ctxt = (G.get G.s_ctxt s) in
  case C.walk ctxt n of
    Left _ ->
      let (r, s1) = G.fresh s
          (val, ctxt') = C.walkOn ctxt n (Ref r)
      in (val, G.set G.s_ctxt ctxt' s1)
    Right v -> (v, s)

assert :: State -> Clause -> State
assert s0 (Assert arr) =
  let
    (s, s1) = assertVal (source arr) s0
    (t, s2) = assertVal (target arr) s1
    s3 = G.arrow (Arrow s (predicate arr) t) s2
  in s3
assert s0 (Named (App name args)) =
  case M.lookup name (G.get G.s_mod s0) of
    Nothing -> error $ "missing definition: " ++ name
    Just (UniquePattern pattern, params) ->
      error "not implemented"
    Just (_, params) | length params /= length args ->
      error $ "argument mismatch: " ++ name
    Just (p@(Pattern _), params) ->
      let s1 = pushFrame s0
          s2 = foldr (uncurry G.bind) s1 (zip params args)
      in popFrame $ solveAssert s2 p
assert s0 (All negative positive) =
  let env = G.getEnv s0
      ctxt = G.get G.s_ctxt s0
      matches = solve env negative ctxt

      s1 = foldl (\s c -> solveAssert (G.set G.s_ctxt c s) positive)
                 s0 matches
  in G.set G.s_ctxt ctxt s1

-- TODO unique asserts
solveAssert :: State -> Pattern -> State
solveAssert s (Pattern cs) = foldl assert s cs
solveAssert _ (UniquePattern _) = error "not implemented"

pushFrame = G.on G.s_ctxt C.push
popFrame = G.on G.s_ctxt C.pop
resetContext = G.set G.s_ctxt C.empty

assertCommand :: State -> AssertPattern -> State
assertCommand s0 (AssertPattern positive) =
  solveAssert s0 (Pattern positive)

commandStep :: State -> Command -> ([String], State)
commandStep s0 c =
  let (msgs, s1) = commandStep' (resetContext s0) c
  in (msgs, s1)

commandStep' s0 (CQuery q) =
  (showResult $ solve (G.getEnv s0) q C.empty, s0)
  where
    showResult [] = ["[]"]
    showResult xs = map show xs
commandStep' s0 (CBinding b@(Binding name _ _)) =
  (["Binding " ++ name], G.bindMod b s0)
commandStep' s0 (CAssert a) = ([], assertCommand s0 a)

execProgram :: Graph -> Program -> ([String], State)
execProgram g p =
  let s0 = G.graphState g
      step (acc, s0) c =
        let (msgs, s1) = commandStep s0 c in (reverse msgs ++ acc, s1)
      (msgs, s1) = foldl step ([], s0) (commands p)
  in (reverse msgs, s1)
