{-# LANGUAGE RecordWildCards #-}
module Types
  (Map
  , Lit(..), Ref(..)
  , Name, Term(..), C
  , Modifier(..)
  , Token(..)
  , Frame(..), FrameType(..)
  , Context, Module, Graph, Env(..)

  , pp

  , fromJust, fromMaybe, mapMaybe, maybeList
  ) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, fromJust, mapMaybe)
import Data.List (intercalate)
import Data.String

data Lit
  = LInt Int
  | LSymbol String
  deriving (Eq, Ord, Show)

data Ref = R Int
  deriving (Eq, Ord, Show)

data Term
  = Lit Lit
  | Ref Ref
  deriving (Eq, Ord, Show)

instance IsString Term where
  fromString = Lit . LSymbol

-- unique
-- all
--
-- posit
-- erase
data Modifier = None | Unique | Posit | Erase
  deriving (Eq, Ord, Show)

data FrameType = MM Modifier | Must | All | Commit
  deriving (Eq, Ord, Show)

data Token
  = {- var -} Variable String
  | {- `+-!-} Modifier' Modifier
  | {- [ -} OpenFrame'
  | OpenFrame Modifier
  -- | Arrow' String
  | Arrow Modifier String
  | {- ] -} CloseFrame
  | {- @ -} Apply'
  | Apply String
  | {- | -} Choice
  | {- . -} Dot
  | {- -> -} Requires
  | {- 'sym -} Symbol'
  | Symbol String
  | Error [String]
  deriving (Eq, Ord, Show)

data Def = Def String [String] [Token]
  deriving (Eq, Ord, Show)


type Module = Map String Def
type Graph = Map String [(Term, Term)]
type Name = String
type Context = Map Name Term
data Env = Env
  { defs :: Module
  , graph :: Graph
  , freshName :: Int
  , context :: Context
  , stack :: [Token]
  }
  deriving (Eq, Ord, Show)

type C = Env

data Frame = Branch C FrameType [Frame] | Leaf C | Done [C]
  deriving (Eq, Ord, Show)


-- Utilities
maybeList Nothing = []
maybeList (Just a) = a

wn n = replicate n ' '
pp n (Branch _ t []) =
  wn n ++ show t ++ " <empty>."
pp n (Branch _ t fs) = unlines $
  [ wn n ++ show t ]
  ++ map (pp (n+2)) fs
pp n (Leaf e) = wn n ++ ppe e
pp n (Done cs) = unlines $
  [ wn n ++ "error." ]
  ++ map ((wn n ++) . ppe) cs

ppe Env{..} = intercalate ", " $
  [ show stack, show (M.toList context)
  -- , show (M.toList graph)
  ]
