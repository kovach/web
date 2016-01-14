module Types where
import qualified Data.Map as M
import Data.Maybe (fromMaybe, fromJust)

-- Types
type Symbol = String
type Sym = Symbol

data Ref = R Int
  deriving (Eq, Ord)

type Edge = (Value, Value)
data Web = Web
  { count :: Int
  , edges :: M.Map Symbol [Edge]
  }
  deriving (Show, Eq, Ord)

data Node = NRoot Symbol | NSym Symbol | NInt Int | NHole
  deriving (Show, Eq, Ord)

data Atom = P Node Symbol Node
  deriving (Show, Eq, Ord)

data Max = Max Symbol Symbol
  deriving (Show, Eq, Ord)

-- Base language
-- operations:
--   - match atom
--   - count
--   - max
--   - forget
-- effects:
--   - new entity
--   - new atom
--   - remove entity and all dependent atoms
data Operation
    = OMatch Atom -- | OF Fold
    | OCount Symbol | OMax Max | ODrop Symbol
  deriving (Show, Eq, Ord)

data Token = TSym Symbol | TInt Int
  deriving (Show, Eq, Ord)

data Effect
    = EFresh Symbol
    | EAssert Token Symbol Token
    | EDel Symbol
  deriving (Show, Eq, Ord)

type LHS = [Operation]
type RHS = [Effect]

type Rule = (LHS, RHS)

data Application = App Symbol [Symbol]
  deriving (Show, Eq, Ord)

data Operation'
    = OOperation Operation
    | ONamed Application
  deriving (Show, Eq, Ord)

-- TODO
data Effect'
    = EEffect Effect
    | ENamed Symbol [Symbol]
  deriving (Show, Eq, Ord)

type Rule' = ([Operation'], [Effect])

data Value = VRef Ref | VInt Int
  deriving (Eq, Ord)

data VType = VTRef | VTInt

type Context = [(Symbol, Value)]
type TContext = [(Symbol, VType)]

type Var = Value -> Either String (Context -> Context)

type RuleContext = [(Symbol, (Rule', [Symbol]))]

data Program = Prog
  { p_defs :: RuleContext
  , p_rule :: Rule'
  }
  deriving (Show, Eq, Ord)

-- Stuff
instance Show Ref where
  show (R i) = "#" ++ show i

instance Show Value where
  show (VRef r) = show r
  show (VInt i) = show i

vint (VInt i) = i

look k web = fromMaybe [] (M.lookup k web)
look' k ctxt = fromJust (lookup k ctxt)

-- TODO delete
insertList :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
insertList k v [] = [(k, v)]
insertList k v ((n, _) : rs) | n == k = (k, v) : rs
insertList k v (r : rs) = r : insertList k v rs

addEdge :: Symbol -> Edge -> Web -> Web
addEdge pred e (Web c env) = Web c (M.insertWith (++) pred [e] env)

-- TODO make monadic?
class Named a where
  nmap :: (Symbol -> Symbol) -> a -> a
instance Named Node where
  nmap _ NHole = NHole
  nmap f (NSym s) = NSym (f s)
  nmap f (NRoot s) = NRoot (f s)
  nmap f (NInt v) = NInt v
instance Named Token where
  nmap f (TSym s) = TSym (f s)
  nmap f (TInt v) = TInt v
instance Named Atom where
  nmap f (P l p r) = P (nmap f l) p (nmap f r)
instance Named Max where
  nmap f (Max a b) = Max (f a) (f b)

instance Named Operation where
  nmap f (OMatch atom) = OMatch (nmap f atom)
  nmap f (OCount s) = OCount (f s)
  nmap f (OMax m) = OMax (nmap f m)
  nmap f (ODrop s) = ODrop (f s)

instance Named Effect where
  nmap f (EFresh s) = EFresh (f s)
  nmap f (EAssert a p b) = EAssert (nmap f a) p (nmap f b)
  nmap f (EDel s) = EDel (f s)

instance Named Operation' where
  nmap f (OOperation op) = OOperation (nmap f op)
  nmap f (ONamed (App n args)) = ONamed (App (f n) (map f args))
