module Interpreter where

import qualified Data.Map as M
import Data.Maybe (fromMaybe, fromJust)
import Data.Either (partitionEithers)
import Data.List (foldl', sortOn, maximumBy)
import Data.Function (on)
import Control.Arrow (second)

-- Types
type Symbol = String

data Ref = R Int
  deriving (Eq, Ord)

type Edge = (Ref, Ref)
data Web = Web
  { count :: Int
  , edges :: M.Map Symbol [Edge]
  }
  deriving (Show, Eq, Ord)
data Node = NSym Symbol | NHole
  deriving (Show, Eq, Ord)

data Pattern = P Node Symbol Node
  deriving (Show, Eq, Ord)
data Fold = Fold Symbol
  deriving (Show, Eq, Ord)
data Max = Max Symbol Symbol
  deriving (Show, Eq, Ord)
data Operation
    = OP Pattern | OF Fold
    | OC Symbol | OM Max | OD Symbol
  deriving (Show, Eq, Ord)

data Effect
    = EFresh Symbol
    | EAssert Symbol Symbol Symbol
    | EDel Symbol
  deriving (Show, Eq, Ord)

data Value = VRef Ref | VInt Int
  deriving (Eq, Ord)

type Binding = (Symbol, Value)
type Context = [Binding]

type Var = Ref -> Either String (Context -> Context)

-- Stuff
instance Show Ref where
  show (R i) = "#" ++ show i

instance Show Value where
  show (VRef r) = show r
  show (VInt i) = show i

vint (VInt i) = i

look k web = fromMaybe [] (M.lookup k web)
look' k ctxt = fromJust (lookup k ctxt)

insertList :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
insertList k v [] = [(k, v)]
insertList k v ((n, _) : rs) | n == k = (k, v) : rs
insertList k v (r : rs) = r : insertList k v rs

addEdge :: Symbol -> Edge -> Web -> Web
addEdge pred e (Web c env) = Web c (M.insertWith (++) pred [e] env)

hvar :: Var
hvar = \_ -> Right id
cvar :: Ref -> Var
cvar r = \r' -> if r == r' then Right id else Left "mismatch"
fvar :: Symbol -> Var
fvar name = \r -> Right $ \c -> (name, VRef r) : c

toVar :: Node -> Context -> Var
toVar NHole _ = hvar
toVar (NSym s) c | Just (VRef r) <- lookup s c = cvar r
toVar (NSym s) c | Just _ <- lookup s c = \_ -> Left "non-atomic pattern"
toVar (NSym s) _ = fvar s

-- Interpreter
update :: Var -> Var -> Context -> Edge
       -> Either String Context
update v1 v2 c (s, t) = do
  f1 <- v1 s
  f2 <- v2 t
  return $ f1 . f2 $ c

getStep :: Pattern -> Web -> Context -> [Context]
getStep (P s pred t) web c =
  let es = look pred $ edges web
      v1 = toVar s c
      v2 = toVar t c
      (_, cs) = partitionEithers $ map (update v1 v2 c) es
  in cs

foldStep :: [Symbol] -> [Context] -> [(Context, [Context])]
foldStep names cs = M.toList $ foldl' fold M.empty cs
  where
    fold m c =
      let (key, ctx) = split c
      in M.insertWith (++) key [ctx] m

    split c = split' c ([], [])
    split' [] p = p
    -- requires unique occurrence of names in context!
    split' (b:bs) (l, r)
      | fst b `elem` names = split' bs (l, b:r)
    split' (b:bs) (l, r)   = split' bs (b:l, r)

countStep :: Symbol -> [Context] -> [Context]
countStep s cs =
  let groups = foldStep [s] cs
      fold (c, vals) = (s, VInt (length vals)) : c
  in map fold groups

maxStep :: Max -> [Context] -> [Context]
maxStep (Max base val) cs =
  let groups = foldStep [base, val] cs
      fold (c, pairs) =
        let bval = look' base $
              maximumBy (compare `on` (look' val)) pairs
        in (base, bval) : c
  in map fold groups

dropStep :: Symbol -> [Context] -> [Context]
dropStep s cs =
  let groups = foldStep [s] cs
      fold (c, _) = c
  in map fold groups

-- LHS operation
step :: Web -> [Context] -> Operation -> [Context]
step w cs (OP p) = concatMap (getStep p w) cs
step w cs (OC s) = countStep s cs
step w cs (OM m) = maxStep m cs
step w cs (OD s) = dropStep s cs

-- Mutations
fresh :: Web -> (Ref, Web)
fresh (Web c e) = (R c, Web (c+1) e)

newEdge :: (Symbol, Symbol, Symbol) -> Context -> Web -> Web
newEdge (s, pred, t) ctxt web =
  let
    (VRef r1) = look' s ctxt
    (VRef r2) = look' t ctxt
  in
    addEdge pred (r1, r2) web

-- TODO should edges have uids?
delNode :: Ref -> Web -> Web
delNode ref (Web c e) =
    Web c (M.map (filter disjoint) e)
  where
    disjoint (r1, r2) | r1 == ref || r2 == ref = False
    disjoint _ = True

-- RHS operation
stepEff :: (Context, Web) -> Effect -> (Context, Web)
stepEff (c, web) (EFresh name) =
  let (r, web') = fresh web
  in (insertList name (VRef r) c, web')

stepEff (c, web) (EAssert s p t) = (c, newEdge (s,p,t) c web)
stepEff (c, web) (EDel name) | VRef r <- look' name c =
  (c, delNode r web)
