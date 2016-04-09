{-# LANGUAGE DeriveGeneric #-} -- needed for json parsing
module Types where
import GHC.Generics (Generic)

import qualified Data.Map as M
import Data.Maybe (fromMaybe, fromJust)

-- a Symbol gets bound; a Sym is a sort of literal
type Symbol = String
data Sym = Sym String
  deriving (Eq, Ord, Generic)

instance Show Sym where
  show (Sym s) = '\'' : s

data Ref = R Int
  deriving (Eq, Ord, Generic)

data Lit
  = LInt Int
  | LSym Sym
  deriving (Show, Eq, Ord, Generic)

data Expr
  = ELit Lit
  | ESym Symbol
  | ERef Ref
  deriving (Show, Eq, Ord, Generic)

data Arrow = Arrow
  { source :: Expr
  , pred ::Symbol
  , target :: Expr }
  deriving (Eq, Ord, Show, Generic)

data Application = App Symbol [Expr]
  deriving (Show, Eq, Ord)

-- TODO name type
-- ? >/<
-- combine ref into it
-- context structure
--  ? context = (Linear One, [Symbol, Val], [Ref, Val])

data Effect
    = Assert Arrow
    | Del Expr
    | ENamed Application
  deriving (Show, Eq, Ord)

data Log = Log Int [Effect]

type Edge = (Expr, Expr)
data Graph = Graph (M.Map Symbol [Edge])
  deriving (Show, Eq, Ord)

data Max = Max Symbol Symbol
  deriving (Show, Eq, Ord)

--data Operation
--    = OMatch Atom
--    | OCount Symbol | OMax Max | ODrop Symbol
--    | ONamed Application
--  deriving (Show, Eq, Ord)

type Pattern = [Effect]

type Rule = (Pattern, Pattern)

type Context = [(Symbol, Expr)]

type Var = Expr -> Either String (Context -> Context)

type RuleContext = [(Symbol, (Pattern, [Symbol]))]

data Program = Prog
  { p_defs :: RuleContext
  , p_rule :: Rule
  }
  deriving (Show, Eq, Ord)

instance Show Ref where
  show (R i) = "#" ++ show i

look k web = fromMaybe [] (M.lookup k web)
look' k ctxt = fromJust (lookup k ctxt)

-- TODO make monadic?
class Named a where
  nmap :: (Symbol -> Symbol) -> a -> a
--instance Named Node where
--  nmap _ NHole = NHole
--  nmap f (NSym s) = NSym (f s)
--  nmap f (NRoot s) = NRoot (f s)
--  nmap f (NLit v) = NLit v
instance Named Expr where
  nmap f (ESym s) = ESym (f s)
  nmap f (ELit v) = ELit v
--instance Named Atom where
--  nmap f (Atom l p r) = Atom (nmap f l) p (nmap f r)
--instance Named Max where
--  nmap f (Max a b) = Max (f a) (f b)

instance Named Application where
  nmap f (App s es) = App s (map (nmap f) es)

--instance Named Operation where
--  nmap f (OMatch atom) = OMatch (nmap f atom)
--  nmap f (OCount s) = OCount (f s)
--  nmap f (OMax m) = OMax (nmap f m)
--  nmap f (ODrop s) = ODrop (f s)
--  nmap f (ONamed (App n args)) = ONamed (App (f n) (map (nmap f) args))

instance Named Arrow where
  nmap f (Arrow a p b) = Arrow (nmap f a) p (nmap f b)
instance Named Effect where
  nmap f (Assert a) = Assert (nmap f a)
  nmap f (Del s) = Del (nmap f s)
  nmap f (ENamed (App n args)) = ENamed (App (f n) (map (nmap f) args))

-- TODO
instance Num Expr where
  fromInteger = ELit . LInt . fromIntegral
  (ELit (LInt l)) + (ELit (LInt r)) = ELit (LInt (l+r))
  negate (ELit (LInt l)) = (ELit (LInt $ -l))
