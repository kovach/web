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

pnames NHole = []
pnames (NSym s) = [s]
pnames (NRoot s) = [s]

names (OMatch (Atom l _ r)) = pnames l ++ pnames r
names (OCount s) = [s]
names (OMax (Max s1 s2)) = [s1, s2]
names (ODrop s) = [s]

updateContext :: Operation -> TContext -> TContext
-- this isn't really right
-- but it isn't used yet
updateContext o@(OMatch _) c = zip (names o) (repeat VTRef) ++ c
updateContext (OCount s) c = (s, VTInt) : c
updateContext (OMax (Max _ d)) c = filter ((/= d) . fst) c
updateContext (ODrop d) c = filter ((/= d) . fst) c

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

type CMod = (Fn TContext, [Operation])
ostep :: RuleContext -> CMod -> Operation' -> (CMod, [Operation'])
ostep _ _ (OOperation op) = ((Fn $ updateContext op, [op]), [])
ostep rc (Fn c, _) (ONamed app@(App name args)) | Nothing <- lookup name rc = ((mempty, [OExtern app]), [])
ostep rc (Fn c, _) (ONamed (App name args)) =
    let context = c []
        ((pattern, []), params) = look' name rc
    in (mempty, fix context params args pattern)
  where
    fix context ps args pattern = map (psub (zip ps args)) pattern
      where
        subE ctxt (ESym s) | Just e <- lookup s ctxt = e
        subE _ e = e
        --psub :: Context -> Operation' -> Operation'
        psub ctxt (ONamed (App sym args)) = ONamed $ App sym (map (subE ctxt) args)
        psub ctxt (OOperation o) = OOperation $
          case o of
            OMatch (Atom l pred r) -> OMatch $ Atom (sub l) pred (sub r)
            -- TODO
            o -> o
          where sub (NSym s) | Just v <- lookup s ctxt =
                  case v of
                    ELit l -> NLit l
                    ESym s -> NSym s
                sub n = n
        -- rename s =
        --   case lookup s (zip ps args) of
        --     Nothing -> freshen (map fst context) s
        --     Just arg -> arg

-- Main function
normalize :: RuleContext -> Rule' -> Rule
normalize rc (ops', effs) = (ops, effs)
  where
    (c, _) = mfix (mstep osplit (ostep rc)) (mempty, ops')
    ops = snd $ c

