module Types
  (Map
  , Name, Term(..), C
  , Modifier(..)
  , Token(..)
  , Frame(..), FrameType(..)
  , Module, Graph, Env(..)

  , fromJust, fromMaybe
  ) where
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, fromJust)

type Name = String
data Term = A | B
  deriving (Eq, Ord, Show)

type C = Map Name Term

--data LToken a
--  = Eq Name Name
--  | Bind Name Term
--  | Split C C
--  | Open a
--  | Close
--  | Arrow
--  deriving (Eq, Ord, Show)

data Modifier = None | Unique | Posit | Erase
  deriving (Eq, Ord, Show)

data FrameType = MM Modifier | Must | All | Commit
  deriving (Eq, Ord, Show)

data Token
  = {- var -} Variable String
  | {- [`+-!]arr -} Arrow Modifier String
  | {- [ -} OpenFrame Modifier
  | {- ] -} CloseFrame
  | {- @ -} Apply String
  | {- | -} Choice
  | {- . -} Dot
  | {- -> -} Implies
  | {- 'sym -} Symbol String
  | Error [String]
  deriving (Eq, Ord, Show)

data Frame = Branch C FrameType [Frame] | Leaf C | Done [C]
  deriving (Eq, Ord, Show)

type Pattern = [Token]

data Def = Def String [String] Pattern
  deriving (Eq, Ord, Show)

type Module = Map String Def
type Graph = Map String [(Term, Term)]
data Env = Env
  { defs :: Module
  , graph :: Graph
  , freshName :: Int
  , frame :: Frame
  , stack :: [Name]
  }
  deriving (Eq, Ord, Show)

