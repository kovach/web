module Types where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, fromJust)

-- names
-- Name = String
-- NL = [String]

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

data Graph = G
  { props :: Map Ref [(Rel, Ref)]
  , edges :: Map Rel [(SRef, SRef)]
  }

data Arrow = Arrow
  { source :: SRef
  , predicate   :: Rel
  , target :: SRef }
  deriving (Eq, Ord, Show)

data Application = App Name [SRef]
  deriving (Show, Eq, Ord)

data Clause
    = Assert Arrow
    | Del SRef
    | Named Application
    | All Pattern Pattern
  deriving (Show, Eq, Ord)

type Pattern = [Clause]

type Context = Map Name SRef
type Env = (Map Name (Pattern, [Name]), Graph)

-- example clauses/named-patterns
--
-- p fst a, p snd b, [x fst a, x fst b] [x map p]
--
-- arrow s arr t:
--   arr source s, arr target t
-- product p a b:
--   @arrow p p-fst a, @arrow p p-snd b,
--     [@arrow x x-fst a, @arrow x x-snd a]
--     [@arrow x factor p,
--      @comp factor p-fst x-fst,
--      @comp factor p-snd x-snd]
--
-- want forall and also forall-unique?
-- want unique subpattern match?

-- TODO ternary relations?
g1 = G {props = M.empty,
  edges = M.fromList $
    [("source", [])
    ,("target", [])
    ]
  }
