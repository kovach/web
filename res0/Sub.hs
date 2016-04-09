{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Sub where

import Types
import Control.Monad.State

freshen :: [Symbol] -> Symbol -> Symbol
freshen cs s | not (s `elem` cs) = s
freshen cs s = go 0 s
  where
    go c n =
      let n' = n ++ show c
      in if n' `elem` cs then go (c+1) n else n'

type Shift = [(Symbol, Symbol)]
type Interface = [(Symbol, Expr)]

class Monad m => Fresh m where
  freshName :: Symbol -> m Symbol
  rename :: Symbol -> m (Maybe Symbol)

type DefaultNameMonad = State (Int, [Symbol], Shift)

instance Fresh DefaultNameMonad where
  freshName n = do
    (c, ns, s) <- get
    if False && (not $ n `elem` ns)
      then return n
      else do
        let n' = n ++ show c
        put (c+1, n':ns, (n,n'):s)
        return n'
  rename s = do
    (_, _, sh) <- get
    return $ lookup s sh

runName :: DefaultNameMonad a -> a
runName = flip evalState (0, [], [])

shift :: Fresh m => Interface -> Expr -> m Expr
shift i (ESym name) | Just e' <- lookup name i = return e'
shift i (ESym name) = do
  mname <- rename name
  case mname of
    Just n' -> return $ ESym n'
    Nothing -> ESym <$> freshName name

shift _ e = return e

shiftEff :: Fresh m => Interface -> Effect -> m Effect
shiftEff i (Assert (Arrow s p t)) = do
  s <- shift i s
  t <- shift i t
  return $ Assert (Arrow s p t)
shiftEff i (Del e) = Del <$> shift i e
shiftEff i (ENamed (App s args)) = do
  args <- mapM (shift i) args
  return $ ENamed (App s args)

shiftClauses :: Fresh m => RuleContext -> Effect -> m [Effect]
shiftClauses rc (ENamed (App s args)) =
  case lookup s rc of
    Just (clauses, params) ->
      mapM (shiftEff (zip params args)) clauses
shiftClauses _ e = return [e]

normalizeRule :: RuleContext -> Rule -> Rule
normalizeRule rc (l, r) = (normalize rc l, normalize rc r)

normalize :: RuleContext -> Pattern -> Pattern
normalize rc p = runName $ fix p
  where
    step = fmap concat . mapM (shiftClauses rc)
    fix x = do
      x' <- step x
      if x == x' then return x else fix x'
