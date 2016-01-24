-- modules for defining derived relations
module Extern where

import Control.Monad (foldM)
import Data.Maybe (mapMaybe)
import Data.Either (partitionEithers)

import Types

-- Marks whether parameter is 'input' or 'output'
-- cf mercury-lang
data Sign = N | P
  deriving (Show, Eq, Ord)

type Mode = [Sign]

type Binding = ([Value], [Symbol])
type Fn = Web -> Binding -> [Context]
type ModeFn = (Mode, Fn)
type Extern = [ModeFn]

data ExtBind = ExtBind Extern [(Symbol, Symbol)]

guard False = Nothing
guard _ = return ()

mkm _ (ELit l) = Left $ VLit l
mkm ctxt (ESym arg) = case lookup arg ctxt of
  Nothing -> Right arg
  Just v -> Left v

signs = map step
  where
    step (Right _) = P
    step (Left _) = N

mkSig args ctxt = map (mkm ctxt) args

takeFirst f xs = case mapMaybe f xs of
    [] -> Nothing
    a : _ -> Just a

matchSig sig (mode, fn) = do
  guard (signs sig == mode)
  return $ (partitionEithers sig, fn)

matchExtern :: Web -> Context -> [Expr] -> Extern -> Maybe [Context]
matchExtern w ctxt args cases = do
  (b, fn) <- takeFirst (matchSig (mkSig args ctxt)) cases
  return $ map (++ ctxt) $ fn w b

-- Actual definitions
plusNNNfn, plusPNNfn, plusNPNfn, plusNNPfn :: Fn
plusNNNfn _ ([l,r,out], []) = if out == l + r then [[]] else []
plusNNPfn _ ([l, r], [out]) = [[(out, l+r)]]
plusNPNfn _ ([l, out], [r]) = [[(r, out-l)]]
plusPNNfn _ ([r, out], [l]) = [[(l, out-r)]]

plus :: Extern
plus = [([N,N,P], plusNNPfn)
       ,([N,N,N], plusNNNfn)
       ,([N,P,N], plusNPNfn)
       ,([P,N,N], plusPNNfn)]

gNN _ ([l, r], []) = if l > r then [[]] else []
greater :: Extern
greater = [([N,N], gNN)]

-- prelude
std_lib = [("+", plus)
          ,(">", greater)]
