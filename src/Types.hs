module Types where
import qualified Data.Map as M
import Data.Maybe (fromMaybe, fromJust)

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

data Node = NRoot Symbol | NSym Symbol | NHole
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

data Effect
    = EFresh Symbol
    | EAssert Symbol Symbol Symbol
    | EDel Symbol
  deriving (Show, Eq, Ord)

type Rule = ([Operation], [Effect])

-- TODO
-- add syntax for named things
data Operation'
    = OOperation Operation
    | ONamed Symbol [Symbol]
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

class Named a where
  nmap :: (Symbol -> Symbol) -> a -> a
instance Named Node where
  nmap _ NHole = NHole
  nmap f (NSym s) = NSym (f s)
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
  nmap f (EAssert a p b) = EAssert (f a) p (f b)
  nmap f (EDel s) = EDel (f s)
