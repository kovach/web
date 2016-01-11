module Check where

import Data.List (foldl')

import Types
import Rewrite

type Error = String

checkApplications :: RuleContext -> Rule' -> Maybe Error
checkApplications = undefined

normalize :: RuleContext -> Rule' -> Rule
normalize rc (ops', effs) = (ops, effs)
  where
    (c, _) = mfix (mstep osplit (ostep rc)) (mempty, ops')
    ops = snd $ c

subOps :: Int -> [Operation'] -> [Operation]
subOps i [] = []
subOps i ((OOperation op) : _) =  undefined

subEffs :: Int -> [Effect'] -> [Effect]
subEffs = undefined

withContext :: (s -> TContext -> a -> s) -> (a -> Maybe Operation)
            -> s -> [a] -> (s, TContext)
withContext step op s = foldl' st (s, [])
  where
    st (s, c) a =
      let c' = maybe id updateContext (op a) $ c
          s' = step s c' a
      in (s', c')
