module Types where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, fromJust)

type Name = String
type Rel = String

data Symbol = Symbol String
  deriving (Eq, Ord, Show)

data Ref = R Int
  deriving (Eq, Ord, Show)

data Lit
  = LInt Int
  | LSymbol Symbol
  deriving (Eq, Ord, Show)

data SRef
  = Lit Lit
  | Ref Ref
  | SName Name
  | SSub SRef [Rel]
  deriving (Eq, Ord, Show)

data Graph = Graph
  { edges :: Map Rel [(SRef, SRef)] }
  deriving (Eq, Ord, Show)

data Arrow = Arrow
  { source      :: SRef
  , predicate   :: Rel
  , target      :: SRef }
  deriving (Eq, Ord, Show)

data Application = App Name [SRef]
  deriving (Show, Eq, Ord)

data Clause
    = Assert Arrow
    | Del [SRef]
    | Named Application
    | All Pattern Pattern
    | SubPattern Pattern
  deriving (Show, Eq, Ord)

data Pattern
  = Pattern [Clause]
  | UniquePattern [Clause]
  deriving (Eq, Ord, Show)
data AssertPattern
  = AssertPattern [Clause] -- TODO move this?
  deriving (Show, Eq, Ord)

type Context = Map Name SRef
emptyContext = M.empty

type Module = Map Name (Pattern, [Name])
type Env = (Module, Graph)

data Binding = Binding Name [Name] Pattern
  deriving (Eq, Ord, Show)

data Command
  = CBinding Binding
  | CQuery Pattern
  | CAssert Pattern AssertPattern
  deriving (Eq, Ord, Show)

bindingName (Binding n _ _) = n

data Program = Program
  { commands :: [Command] }
  deriving (Eq, Ord, Show)
