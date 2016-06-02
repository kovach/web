module Syntax where

import Types

import qualified Data.Map as M
import Data.Char (isAlphaNum)

data PreEnv = PreEnv
  { definitions :: [String]
  , context :: [String]
  }
  deriving (Eq, Ord, Show)

delims :: String
delims = "[]-+!@|`.'>"

fixChar :: Char -> String
fixChar c | c `elem` delims = [' ', c, ' ']
fixChar c = [c]

alphaNum = all isAlphaNum

tokenize :: [String] -> ([Token], [String])
tokenize ("[" : ts) = ([OpenFrame None], ts)
tokenize ("." : ts) = ([Dot], ts)
tokenize ("-" : ">" : ts) = ([Implies], ts)
tokenize ("`" : t : ts) | alphaNum t = ([Arrow None t], ts)
tokenize ("+" : t : ts) | alphaNum t = ([Arrow Posit t], ts)
tokenize ("-" : t : ts) | alphaNum t = ([Arrow Erase t], ts)
tokenize ("!" : t : ts) | alphaNum t = ([Arrow Unique t], ts)
tokenize ("+" : "[" : ts) = ([OpenFrame Posit], ts)
tokenize ("-" : "[" : ts) = ([OpenFrame Erase], ts)
tokenize ("!" : "[" : ts) = ([OpenFrame Unique], ts)
tokenize ("]" : ts) = ([CloseFrame], ts)
tokenize ("|" : ts) = ([Choice], ts)
tokenize ("@" : t : ts) | alphaNum t = ([Apply t], ts)
tokenize ("'" : t : ts) | alphaNum t = ([Symbol t], ts)
tokenize (t : ts) | alphaNum t = ([Variable t], ts)
tokenize ts = ([], ts)

tokens :: String -> [Token]
tokens s =
    let ts = words . concatMap fixChar $ s
    in reduce ts
  where
    reduce [] = []
    reduce ts = case tokenize ts of
      ([], _) -> [Error ts]
      (t, ts') -> t ++ reduce ts'

ex0 = ""
ex1 = "a c ``f"
ex2 = "a b @p [a c +arr]"
exercises =
  [ ex0
  , ex1
  , ex2
  ]
 

main = do
  mapM_ (print . tokens) exercises
