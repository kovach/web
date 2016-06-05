{-# LANGUAGE RecordWildCards #-}
import Types

import qualified Data.Map as M
import qualified Syntax as S
import Unification
import Debug.Trace (trace)

g0 = M.fromList
    [ ("p", [("x", "y"), ("y", "x")])
    , ("q", [("y", "x")])
    ]
emptyEnv :: Env
emptyEnv = Env
  { defs = M.empty
  , graph = M.empty
  , freshName = 0
  , context = M.empty
  , stack = []
  }

resetEnv e = e{context = M.empty, stack = []}

data Sign = OK [Frame] | Bounce
          | Fail C String
          | Null
  deriving (Eq, Ord, Show)

instance Monoid Sign where
  OK as `mappend` OK bs = OK (as++bs)
  f@(Fail _ _) `mappend` _ = f
  _ `mappend` f@(Fail _ _) = f
  Bounce `mappend` _ = Bounce
  _ `mappend` Bounce = Bounce
  a `mappend` Null = a
  Null `mappend` a = a
  mempty = Null

freshFrame m l@(Leaf c) = Branch c m [l]

emptyF (Branch _ _ []) = True
emptyF _ = False

closeFrame c m bs =
  case m of
    MM None -> OK bs
    Commit -> OK bs
    MM Unique -> if length bs == 1 then OK bs else OK []
    Must -> Bounce
    All -> if any emptyF bs then OK [] else OK [Leaf c]

step :: Token -> Frame -> Sign
-- We enter Commit mode after a successful branch of a disjunction
step t b@(Branch c Commit bs) =
  case t of
    CloseFrame -> closeFrame c Commit bs
    _ -> OK [b]
-- This branch has failed (it has no children).
-- Only difference between this and the `Bounce` branch below is in handling `Choice`
step t b@(Branch c m []) =
  -- Need to handle the instructions that would have Bounced
  case t of
    CloseFrame -> closeFrame c m []
    -- restart
    Choice -> OK [Branch c m [Leaf c]]
    Requires -> OK [Branch c All []]
    _ -> OK [b]
step t b@(Branch c m bs) =
  case mconcat (map (step t) bs) of
    -- We have no children. This case is handled already
    Null -> error "impossible."
    Bounce -> case t of
      CloseFrame -> closeFrame c m bs
      Requires -> OK [Branch c All (map (freshFrame Must) bs)]

      -- We must have a successful match for this clause (since we didn't hit
      -- the `Null` branch above) so we enter `Commit` mode and ignore further
      -- instructions until CloseFrame
      Choice -> OK [Branch c Commit bs]

      -- Error -> _

    f@(Fail _ _) -> f
    OK bs' -> OK $ [Branch c m bs']
step t l@(Leaf c@(Env{..})) = case t of
  v@(Variable _) -> OK [Leaf c {stack = v:stack}]
  s@(Symbol _)   -> OK [Leaf c {stack = s:stack}]
  OpenFrame m ->
    case m of
      _ -> OK $ [freshFrame (MM m) l]

  --Apply fn -> _
  Arrow mod arr -> case stack of
    right : left : s ->
      let c0 = c {stack = s} in
      case mod of
        None -> 
          let pairs = map (namePair left right) $ look arr graph
              ctxts = mapMaybe (unify context) pairs
              newLeaf ctxt = Leaf $ c0 {context = ctxt}
              cs = map newLeaf ctxts
          in OK cs
        Posit ->
          let (l, c1) = freshTerm left c0
              (r, c2) = freshTerm right c1
              c3 = addEdge arr l r c2
          in OK [Leaf c3]
        _ -> error "arrow modifiers not implemented."
    _ -> Fail c "missing arrow variable."
  --Dot -> _
  --Symbol sym -> _

  CloseFrame -> Bounce
  Choice -> Bounce
  Requires -> Bounce

  --TODO remove
  --Bind n v -> OK $ [Leaf $ M.insert n v c]
  --Split a b -> OK $ [Leaf (c`mappend`a), Leaf (c`mappend`b)]
  --Eq m n -> if M.lookup m c == M.lookup n c then OK [l] else OK []

  -- Error -> _

fromLeaf (Leaf x) = x

reduce f [] = [f]
reduce f (t:ts) =
  case step t f of
    OK [x] -> f : reduce x ts
    OK xs -> error "not a singleton." -- concatMap (flip reduce ts) xs
    Fail c m -> [Done [c]]

isComment ('#' : _) = False
isComment _ = True

main = do
  file <- readFile "res2/exercises2"
  let 
      ex0 = filter isComment . filter (not . null) $ lines file
      ex1 = map S.tokens ex0

      doex :: Env -> [[Token]] -> IO ()
      doex _ [] = return ()
      doex c0 (e:es) = do
        let f0 = freshFrame (MM None) $ Leaf $ c0
        print e
        let f' = reduce f0 e
        let l = last $ f'
        mapM_ (putStrLn . pp 0) $ f'
        putStrLn "------------"
        let c = case l of
                  Branch _ _ [Leaf c] -> c
                  _ -> c0
        doex (resetEnv c) es

  doex emptyEnv ex1

