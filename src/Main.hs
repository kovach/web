module Main where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Either (partitionEithers)
import Data.List (foldl', sortOn)
import Control.Arrow (second)

type Symbol = String

data Ref = R Int
  deriving (Eq, Ord)

instance Show Ref where
  show (R i) = "#" ++ show i

type Edge = (Ref, Ref)
type Web = M.Map Symbol [Edge]
type Binding = (Symbol, Ref)
type Context = [Binding]

data Node = NSym Symbol | NHole
  deriving (Show, Eq, Ord)

data Pattern = P Node Symbol Node
  deriving (Show, Eq, Ord)

data MM a = Val a | Ok | Fail String

type Var = Ref -> Either String (Context -> Context)

hvar :: Var
hvar = \_ -> Right id
cvar :: Ref -> Var
cvar r = \r' -> if r == r' then Right id else Left "mismatch"
fvar :: Symbol -> Var
fvar name = \r -> Right $ \c -> (name, r) : c

update :: Var -> Var -> Context -> Edge -> Either String Context
update v1 v2 c (s, t) = do
  f1 <- v1 s
  f2 <- v2 t
  return $ f1 . f2 $ c

look k web = fromMaybe [] (M.lookup k web)

toVar :: Node -> Context -> Var
toVar NHole _ = hvar
toVar (NSym s) c | Just r <- lookup s c = cvar r
toVar (NSym s) _ = fvar s

get :: Pattern -> Web -> Context -> [Context]
get (P NHole pred NHole) _ _ = []
get (P s pred t) web c =
  let es = look pred web
      v1 = toVar s c
      v2 = toVar t c
      (_, cs) = partitionEithers $ map (update v1 v2 c) es
  in cs

toWeb :: [(String, [(Int, Int)])] -> Web
toWeb = M.fromList . map (second (map (\(a, b) -> (R a, R b))))

testWeb = toWeb $
  [ ("x", [(0, 2), (1, 2)])
  , ("y", [(2, 3), (2, 4)])
  , ("z", [(3, 5)])
  ]

top :: String -> Pattern
top str | [".", p, "."] <- words str = P NHole p NHole
top str | [".", p, t] <- words str = P NHole p (NSym t)
top str | [s, p, "."] <- words str = P (NSym s) p NHole
top str | [s, p, t] <- words str = P (NSym s) p (NSym t)

split = lines . map fix
  where
    fix ',' = '\n'
    fix x = x

showCtxt :: Context -> Context
showCtxt = sortOn fst

run :: String -> [Context] -> [Context]
run prog c =
  foldl' (\cs p -> concatMap (get (top p) testWeb) cs)
         c (split prog)

chk prog = do
  putStrLn prog
  mapM_ (print . showCtxt) $ run prog [[]]

main = do
  let p1 = "a x b, b y c, c z d"
      p2 = "a x b, b y c"
  chk p1
  chk p2
