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

tokenize :: String -> Maybe Token
tokenize "[" = Just $ OpenFrame'
tokenize "." = Just $ Dot
tokenize ">" = Just $ Requires
tokenize "`" = Just $ Modifier' None
tokenize "+" = Just $ Modifier' Posit
tokenize "-" = Just $ Modifier' Erase
tokenize "!" = Just $ Modifier' Unique
tokenize "]" = Just $ CloseFrame
tokenize "|" = Just $ Choice
tokenize "@" = Just $ Apply'
tokenize "'" = Just $ Symbol'
tokenize t | alphaNum t = Just $ Variable t
tokenize _ = Nothing

tail' [] = []
tail' x = tail x

reduce :: [Token] -> Maybe [Token]
reduce (Variable v : Modifier' m : s) = Just $ Arrow m v : s
reduce (OpenFrame' : Modifier' m : s) = Just $ OpenFrame m : s
reduce (OpenFrame' : s) = Just $ OpenFrame None : s
reduce (Variable v : Apply' : s) = Just $ Apply v : s
reduce (Variable v : Symbol' : s) = Just $ Symbol v : s
reduce t | hasMod (tail' t) = Nothing
  where hasMod [] = False
        hasMod (Modifier' _ : _) = True
        hasMod (_ : t) = hasMod t
reduce t | Apply' `elem` (tail' t) = Nothing
reduce t | Symbol' `elem` (tail' t) = Nothing
reduce s = Just s

tokens :: String -> [Token]
tokens s =
    let ts = words . concatMap fixChar $ s
    in step [] ts
  where
    step acc [] = reverse acc
    step acc (t:ts) = case tokenize t of
      Nothing -> reverse $ Error ts : acc
      Just t -> case reduce (t:acc) of
        Just a' -> step a' ts
        Nothing -> reverse $ Error ts : t:acc

ex0 = ""
ex1 = "a c ``f"
ex2 = "a b @p [a c `a1 a c +a2]"
exercises =
  [ ex0
  , ex1
  , ex2
  ]

main = do
  mapM_ (print . tokens) exercises
