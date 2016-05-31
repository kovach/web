module Base
  (Map
  , Name, Term(..), C
  , Token(..)
  ) where
import Data.Map (Map)
import qualified Data.Map as M

type Name = String
data Term = A | B
  deriving (Eq, Ord, Show)

type C = Map Name Term

data Token a
  = Eq Name Name
  | Bind Name Term
  | Split C C
  | Open a
  | Close
  | Arrow
  deriving (Eq, Ord, Show)
