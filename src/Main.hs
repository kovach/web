module Main where

import qualified Data.Map as M
import Data.Maybe (fromMaybe, fromJust)
import Data.Either (partitionEithers)
import Data.List (foldl', sortOn, maximumBy)
import Data.Function (on)
import Control.Arrow (second)

type Symbol = String

data Ref = R Int
  deriving (Eq, Ord)

instance Show Ref where
  show (R i) = "#" ++ show i

type Edge = (Ref, Ref)
type Web = M.Map Symbol [Edge]
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

data Value = VRef Ref | VInt Int
  deriving (Eq, Ord)

instance Show Value where
  show (VRef r) = show r
  show (VInt i) = show i

vint (VInt i) = i

type Binding = (Symbol, Value)
type Context = [Binding]

insertList :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
insertList k v [] = [(k, v)]
insertList k v ((n, _) : rs) | n == k = (k, v) : rs
insertList k v (r : rs) = r : insertList k v rs

type Var = Ref -> Either String (Context -> Context)

hvar :: Var
hvar = \_ -> Right id
cvar :: Ref -> Var
cvar r = \r' -> if r == r' then Right id else Left "mismatch"
fvar :: Symbol -> Var
fvar name = \r -> Right $ \c -> (name, VRef r) : c

update :: Var -> Var -> Context -> Edge
       -> Either String Context
update v1 v2 c (s, t) = do
  f1 <- v1 s
  f2 <- v2 t
  return $ f1 . f2 $ c

look k web = fromMaybe [] (M.lookup k web)
look' k ctxt = fromJust (lookup k ctxt)

toVar :: Node -> Context -> Var
toVar NHole _ = hvar
toVar (NSym s) c | Just (VRef r) <- lookup s c = cvar r
toVar (NSym s) c | Just _ <- lookup s c = \_ -> Left "non-atomic pattern"
toVar (NSym s) _ = fvar s

-- Interpreter
getStep :: Pattern -> Web -> Context -> [Context]
getStep (P s pred t) web c =
  let es = look pred web
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

step :: Web -> [Context] -> Operation -> [Context]
step w cs (OP p) = concatMap (getStep p w) cs
step w cs (OC s) = countStep s cs
step w cs (OM m) = maxStep m cs
step w cs (OD s) = dropStep s cs

-- Interface
toWeb :: [(String, [(Int, Int)])] -> Web
toWeb = M.fromList . map (second (map (\(a, b) -> (R a, R b))))

testWeb = toWeb $
  [ ("x", [(0, 2), (1, 2)])
  , ("y", [(2, 3), (2, 4)])
  , ("z", [(3, 5)])
  , ("id", [(0, 1), (1, 0), (2, 3)])
  , ("A", [(-1, 6)])
  , ("B", [(-1, 7)])
  , ("names", [ (6, 0), (6, 3)
              , (7, 0), (7, 1), (7, 2)])
  ]

mp a@(Just _) _ = a
mp _ a@(Just _) = a
mp _ _ = Nothing

parse :: String -> Maybe Operation
parse s =
  -- `mp` (OF <$> parseF s)
  (OC <$> parseC s)
  `mp` (OM <$> parseM s)
  `mp` (OD <$> parseD s)
  `mp` (OP <$> parseP s)


parseP :: String -> Maybe Pattern
parseP str | [".", p, "."] <- words str = Just $ P NHole p NHole
parseP str | [".", p, t] <- words str = Just $ P NHole p (NSym t)
parseP str | [s, p, "."] <- words str = Just $ P (NSym s) p NHole
parseP str | [s, p, t] <- words str = Just $ P (NSym s) p (NSym t)
parseP _ = Nothing

parseF :: String -> Maybe Fold
parseF s  | ["fold", p] <- words s = Just (Fold p)
parseF _ = Nothing

parseC :: String -> Maybe Symbol
parseC s  | ["count", p] <- words s = Just p
parseC _ = Nothing

parseM :: String -> Maybe Max
parseM s | [base, "max", v] <- words s = Just (Max base v)
parseM _ = Nothing

parseD :: String -> Maybe Symbol
parseD s | ["drop", v] <- words s = Just v
parseD _ = Nothing

split = lines . map fix
  where
    fix ',' = '\n'
    fix x = x

showCtxt :: Context -> Context
showCtxt = sortOn fst

run :: String -> Maybe [Context]
run prog = do
  ops <- mapM parse $ split prog
  return $ foldl' (step testWeb) [[]] ops

chk prog = do
  putStrLn $ "\n" ++ prog
  case run prog of
    Just cs -> do
      mapM_ (print . showCtxt) $ cs
    _ -> putStrLn "error"

main = do
  let p1 = "a x b, b y c, c z d"
      p2 = "a id b, b id a"
      p5 = "repo names a, count a"
      -- "repo with most elements"
      p6 = "repo names a, count a, repo max a"
      -- "elements of largest repo"
      p7 = "repo names a, count a, repo max a, repo names b, drop repo"

  chk p1
  chk p2
  chk p5
  chk p6
  chk p7
