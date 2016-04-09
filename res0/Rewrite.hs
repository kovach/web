module Rewrite where

import Types

data Fn a = Fn (a -> a)

instance Monoid (Fn a) where
  mappend (Fn f) (Fn g) = Fn (g . f)
  mempty = Fn id

freshen :: [Symbol] -> Symbol -> Symbol
freshen cs s | not (s `elem` cs) = s
freshen cs s = go 0 s
  where
    go c n | not (n ++ show c `elem` cs) = n ++ show c
    go c n = go (c+1) n

--pnames NHole = []
--pnames (NSym s) = [s]
--pnames (NRoot s) = [s]
--
---- TODO delete?
--names (OMatch (Atom l _ r)) = pnames l ++ pnames r
--names (OCount s) = [s]
--names (OMax (Max s1 s2)) = [s1, s2]
--names (ODrop s) = [s]
--names (ONamed (App _ es)) = concatMap enames es

mstep :: (Monoid m, Monoid c)
      => (m -> Maybe (a, m))    -- "split"
      -> (c -> a -> (c, m))     -- "step"
      -> (c, m) -> Maybe (c, m) -- iterator
mstep split step (c0, m0) = do
  (v, m1) <- split m0
  let (c1, m2) = step c0 v
  return $ (mappend c0 c1, mappend m2 m1)

mfix :: (a -> Maybe a) -> a -> a
mfix step a =
  case step a of
    Nothing -> a
    Just a1 -> mfix step a1

-- Pattern substitution implementation
osplit [] = Nothing
osplit (v : vs) = Just (v, vs)

type CMod = [Operation]
ostep :: RuleContext -> CMod -> Operation -> (CMod, [Operation])
ostep _ _ op@(OMatch _) = ([op], [])
ostep _ _ op@(OCount _) = ([op], [])
ostep _ _ op@(OMax _) = ([op], [])
ostep _ _ op@(ODrop _ ) = ([op], [])
ostep rc _ (ONamed app@(App name args)) | Nothing <- lookup name rc = ([ONamed app], [])
ostep rc _ (ONamed (App name args)) =
    let ((pattern, []), params) = look' name rc
    in (mempty, fix params args pattern)
  where
    fix ps args pattern = map (psub (zip ps args)) pattern
      where
        subE :: [(Symbol, Symbol)] -> Expr -> Symbol
        subE ctxt (ESym s) | Just e <- lookup s ctxt = e
        subE ctxt e = freshen (map fst ctxt) e

        e2n :: Expr -> Node
        e2n (ELit l) = NLit l
        e2n (ESym s) = NSym s

        psub :: [(Symbol, Symbol)] -> Operation -> Operation
        psub ctxt (ONamed (App sym args)) = ONamed $ App sym (map (subE ctxt) args)
        psub ctxt (OMatch (Atom l pred r)) = OMatch $ Atom (sub l) pred (sub r)
          where sub (NSym s) | Just v <- lookup s ctxt = e2n v
                sub n = n
        psub ctxt op = op

-- Main function
normalize :: RuleContext -> Rule -> Rule
normalize rc (ops', effs) = (ops, effs)
  where
    (ops, _) = mfix (mstep osplit (ostep rc)) (mempty, ops')
