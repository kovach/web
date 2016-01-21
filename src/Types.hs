module Types where
import qualified Data.Map as M
import Data.Maybe (fromMaybe, fromJust)

-- Types
type Symbol = String
data Sym = Sym String
  deriving (Eq, Ord)

instance Show Sym where
  show (Sym s) = '\'' : s

data Ref = R Int
  deriving (Eq, Ord)

data Lit
  = LInt Int
  | LSym Sym
  deriving (Show, Eq, Ord)

-- Node identifiers
data Value
  -- An entity, or "monad"
  = VRef Ref
  -- Everything below is "mathematical"
  | VLit Lit
  deriving (Eq, Ord)

data Expr
  = ESym Symbol
  | ELit Lit
  deriving (Show, Eq, Ord)

type Edge = (Value, Value)

data Web = Web
  { count :: Int
  , edges :: M.Map Symbol [Edge]
  }
  deriving (Show, Eq, Ord)

data Node = NRoot Symbol | NSym Symbol
          | NLit Lit
          | NHole
  deriving (Show, Eq, Ord)

data Atom = Atom Node Symbol Node
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
    | OExtern Application
  deriving (Show, Eq, Ord)

data Effect
    = EFresh Symbol
    | EAssert Expr Symbol Expr
    | EDel Symbol
  deriving (Show, Eq, Ord)

type LHS = [Operation]
type RHS = [Effect]

type Rule = (LHS, RHS)

data Application = App Symbol [Expr]
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
  show (VLit (LInt i)) = show i
  show (VLit (LSym s)) = show s

vint (VLit (LInt i)) = i

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
  nmap f (NLit v) = NLit v
instance Named Expr where
  nmap f (ESym s) = ESym (f s)
  nmap f (ELit v) = ELit v
instance Named Atom where
  nmap f (Atom l p r) = Atom (nmap f l) p (nmap f r)
instance Named Max where
  nmap f (Max a b) = Max (f a) (f b)

instance Named Operation where
  nmap f (OMatch atom) = OMatch (nmap f atom)
  nmap f (OCount s) = OCount (f s)
  nmap f (OMax m) = OMax (nmap f m)
  nmap f (ODrop s) = ODrop (f s)
  nmap f (OExtern (App n args)) = OExtern (App (f n) (map (nmap f) args))

instance Named Effect where
  nmap f (EFresh s) = EFresh (f s)
  nmap f (EAssert a p b) = EAssert (nmap f a) p (nmap f b)
  nmap f (EDel s) = EDel (f s)

instance Named Operation' where
  nmap f (OOperation op) = OOperation (nmap f op)
  nmap f (ONamed (App n args)) = ONamed (App (f n) (map (nmap f) args))


-- TODO
instance Num Value where
  fromInteger = VLit . LInt . fromIntegral
  (VLit (LInt l)) + (VLit (LInt r)) = VLit (LInt (l+r))
  negate (VLit (LInt l)) = (VLit (LInt $ -l))
