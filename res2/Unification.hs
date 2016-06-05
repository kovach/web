{-# LANGUAGE RecordWildCards #-}
module Unification where

import Types
import qualified Data.Map as M
import Debug.Trace (trace)

unify :: Context -> [(Token, Term)] -> Maybe Context
unify c [] = Just c
unify c ((Symbol s, v):bs) = do
  unifyVal (sym s) v
  unify c bs
unify c ((Variable n,v):bs) = do
  val <- case M.lookup n c of
           Nothing -> return v
           Just v' -> unifyVal v' v
  unify (M.insert n v c) bs

unifyVal a b | a == b = Just a
unifyVal _ _ = Nothing

look :: String -> Graph -> [(Term, Term)]
look v = maybeList . M.lookup v

sym = Lit . LSymbol

namePair left right (l,r) = [(left,l), (right,r)]

freshTerm (Symbol s) c = (sym s, c)
freshTerm (Variable var) c | Just t <- M.lookup var (context c) = (t, c)
freshTerm (Variable var) e@(Env{..}) =
  let f = Ref (R freshName)
  in (f, e{freshName = freshName + 1, context = M.insert var f context})

addEdge arr l r e@(Env{..}) =
  e {graph = M.insertWith (++) arr [(l,r)] graph}
